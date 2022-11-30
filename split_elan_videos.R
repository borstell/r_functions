# Custom script to cut video by ELAN segmentations 

# Input arguments
args <- commandArgs(TRUE)

# Read ELAN file(s) function originally from signglossR
read_eaf <- function(path){
  if (tools::file_ext(path) == "eaf") {
    filenames <- c(gsub(".*/","",path))
    path <- stringr::str_replace(path, gsub(".*/","",path), "")
  }
  else {
    filenames = list.files(path = path, pattern="*.eaf$")
  }
  
  all_annotations <- dplyr::tibble()
  for (f in filenames){
    message(paste0("Reading file: ",f))
    eaf <- xml2::read_xml(paste0(path, f))
    ts <- xml2::xml_find_all(eaf, ".//TIME_SLOT")
    times <- dplyr::tibble(t=xml2::xml_attr(ts, "TIME_SLOT_ID"),
                           time=xml2::xml_attr(ts, "TIME_VALUE"))
    all_tiers <- xml2::xml_find_all(eaf, ".//TIER")
    
    annotations <- dplyr::tibble(file=f,
                                 a=xml2::xml_attr(xml2::xml_children(xml2::xml_children(all_tiers)), "ANNOTATION_ID"),
                                 t1=xml2::xml_attr(xml2::xml_children(xml2::xml_children(all_tiers)), "TIME_SLOT_REF1"),
                                 t2=xml2::xml_attr(xml2::xml_children(xml2::xml_children(all_tiers)), "TIME_SLOT_REF2"),
                                 annotation=xml2::xml_text(xml2::xml_children(xml2::xml_children(all_tiers))),
                                 ref=xml2::xml_attr(xml2::xml_children(xml2::xml_children(all_tiers)), "ANNOTATION_REF"))
    
    a <- c()
    t1 <- c()
    t2 <- c()
    ref <- c()
    annotation <- c()
    tier <- c()
    lingtype <- c()
    participant <- c()
    annotator <- c()
    for (n in xml2::xml_children(xml2::xml_children(all_tiers))){
      a <- c(a, xml2::xml_attr(n, "ANNOTATION_ID"))
      t1 <- c(t1, xml2::xml_attr(n, "TIME_SLOT_REF1"))
      t2 <- c(t2, xml2::xml_attr(n, "TIME_SLOT_REF2"))
      ref <- c(ref, xml2::xml_attr(n, "ANNOTATION_REF"))
      annotation <- c(annotation, xml2::xml_text(n))
      tier <- c(tier, xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(n)),"TIER_ID"))
      lingtype <- c(lingtype, xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(n)),"LINGUISTIC_TYPE_REF"))
      participant <- c(participant, xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(n)),"PARTICIPANT"))
      annotator <- c(annotator, xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(n)),"ANNOTATOR"))
    }
    
    annotations <- dplyr::tibble(
      file=f,
      a,
      t1,
      t2,
      ref,
      annotation,
      tier,
      tier_type=lingtype,
      participant,
      annotator
    )
    
    parent_annotations <- dplyr::select(dplyr::filter(annotations, is.na(ref)),-ref)
    
    child_annotations <- dplyr::select(dplyr::filter(annotations,!is.na(ref)),-c(t1,t2)) 
    child_annotations <- dplyr::left_join(child_annotations, dplyr::select(parent_annotations,a,tier), by=c("ref"="a"))
    child_annotations <- dplyr::rename(child_annotations, 
                                       tier = tier.x, 
                                       parent_tier = tier.y)
    
    file_annotations <- dplyr::bind_rows(parent_annotations,child_annotations)
    file_annotations <- dplyr::left_join(file_annotations, times, by=c("t1"="t"))
    file_annotations <- dplyr::rename(file_annotations, start = time)
    file_annotations <- dplyr::left_join(file_annotations, times, by=c("t2"="t"))
    file_annotations <- dplyr::rename(file_annotations, end = time)
    file_annotations <- dplyr::mutate(file_annotations, 
                                      end = as.numeric(end),
                                      start = as.numeric(start),
                                      duration = end-start)
    
    all_annotations <- dplyr::bind_rows(all_annotations, file_annotations)
  }
  
  return(all_annotations)
}

# Custom split function
split_elan_video <- function(elan_path, segmentation_tier, video_path, annotation_tag=F, trim=0, video_input_format=".mp4", video_output_format=".mp4"){
  
  original_video_path <- video_path
  
  annotations <- read_eaf(path = elan_path) 
  annotations <- dplyr::mutate(annotations,
                               start_time = format(as.POSIXct((start-trim) / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"),
                               end_time = format(as.POSIXct((end+trim) / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3")) 
  annotations <- dplyr::filter(annotations, tier == segmentation_tier)
  
  for (filename in unique(annotations$file)) {
    current_annotations <- dplyr::filter(annotations, file==filename)
    
    for (i in 1:nrow(current_annotations)) {
      
      current_filename <- current_annotations[i,]$file
      
      if (tools::file_ext(original_video_path) != ""){
        bare_video_path <- stringr::str_sub(original_video_path, 1, -5)
        video_input_format <- paste0(".", tools::file_ext(original_video_path))
      } else {
        bare_video_path <- paste0(original_video_path, gsub(".eaf", "", current_filename))
        video_path <- paste0(bare_video_path, video_input_format)
      }
      
      tag <- ""
      if(annotation_tag){
        tag <- paste0("_", stringr::str_replace_all(current_annotations[i,]$annotation, "[^[:alnum:]]", "-"))
      }
      
      segment_path <- paste0(bare_video_path, "_", stringr::str_pad(i, width=3, pad="0"), tag)
      
      ffmpeg_cmd <- paste0("ffmpeg -i ", 
                           video_path, 
                           " -ss ",
                           current_annotations[i,]$start_time, 
                           " -to ", 
                           current_annotations[i,]$end_time,
                           " ",
                           segment_path,
                           video_output_format)
      system(ffmpeg_cmd)
    }
  }
}

if (length(args)==3){
  split_elan_video(elan_path = args[1], segmentation_tier = args[2], video_path = args[3])
} else { 
  if (length(args)==6){
    split_elan_video(elan_path = args[1], segmentation_tier = args[2], video_path = args[3], annotation_tag = as.logical(args[4]), trim = as.numeric(args[5]), video_input_format = args[6])
  }
}
