# Custom functions to cut video by ELAN segmentations
#
#
# Read EAF file(s) function based on function from {tidysigns}



# eaf2df ------------------------------------------------------------------

# eaf2df reads a directory for EAF (.eaf) files and outputs a tibble
eaf2df <- function(path, tiers = c(), glosses = c(), recursive = FALSE) {
  
  # Check that path is an existing file or directory
  stopifnot("Error: Path does not exist!" = (dir.exists(path) | file.exists(path) | (startsWith(path, "http") & endsWith(path, "eaf"))))
  
  # If path is an .eaf file, append to vector
  if (tools::file_ext(path) == "eaf") {
    filenames <- c(path)
  }
  
  # If path is directory, append all .eaf files within it to vector
  else {
    filenames = list.files(path = path,
                           pattern="*.eaf$",
                           full.names = TRUE,
                           recursive = recursive)
  }
  
  # Check that at least one .eaf file is found
  stopifnot("Error: No .eaf files found!" = length(filenames) >= 1)
  
  # Function to read individual .eaf file
  read_eaf1 <- function(f) {
    
    # Read as .xml
    eaf <- xml2::read_xml(f)
    
    # Make tibble from timestamp data
    ts <- xml2::xml_find_all(eaf, ".//TIME_SLOT")
    times <- dplyr::tibble(TIME_SLOT_ID = xml2::xml_attr(ts, "TIME_SLOT_ID"),
                           TIME_VALUE = xml2::xml_attr(ts, "TIME_VALUE"))
    
    # Restrict parse to custom selected tiers only
    stopifnot("Error: No tiers selected!" = length(tiers) > 0)
    tier_attrs <- paste0(".//TIER[", paste0("@TIER_ID='", tiers, "'", collapse = " or "), "]")
    
    # Make tibble from annotations
    annotations <-
      eaf |>
      xml2::xml_find_all(tier_attrs) |>
      xml2::xml_children() |>
      xml2::xml_children()
    
    # Iterate through nodes and find parent attributes
    if (length(annotations) > 0) {
      annotations <-
        annotations |>
        purrr::map(
          \(x)
          c(
            a = xml2::xml_attr(x, "ANNOTATION_ID"),
            ts1 = xml2::xml_attr(x, "TIME_SLOT_REF1"),
            ts2 = xml2::xml_attr(x, "TIME_SLOT_REF2"),
            annotation = xml2::xml_text(x),
            tier = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "TIER_ID"),
            tier_type = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "LINGUISTIC_TYPE_REF"),
            participant = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "PARTICIPANT"),
            parent_ref = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "PARENT_REF"),
            a_ref = xml2::xml_attr(x, "ANNOTATION_REF")
          )
        ) |>
        dplyr::bind_rows() |>
        
        # Mutate columns and join with timestamp data
        dplyr::left_join(dplyr::rename(times, start = dplyr::all_of("TIME_VALUE")), by = dplyr::join_by("ts1" == "TIME_SLOT_ID")) |>
        dplyr::left_join(dplyr::rename(times, end = dplyr::all_of("TIME_VALUE")), by = dplyr::join_by("ts2" == "TIME_SLOT_ID")) |>
        dplyr::mutate(dplyr::across(dplyr::all_of("start"):dplyr::all_of("end"), as.double)) |>
        dplyr::mutate(file = basename(f),
                      duration = .data$end - .data$start) |>
        dplyr::relocate(dplyr::all_of("file"), .before = 1)
      
      if (any(!is.na(annotations$a_ref))) {
        
        # Subset independent (parent) tier annotations
        parent_annotations <-
          annotations |>
          dplyr::filter(is.na(.data$a_ref)) |>
          dplyr::select(-dplyr::all_of("a_ref"))
        
        # Subset dependent (child) tier annotations
        child_annotations <-
          annotations |>
          dplyr::filter(!is.na(.data$a_ref))
        
        child_tiers <- unique(child_annotations$tier)
        
        child_annotations <-
          child_annotations |>
          dplyr::select(dplyr::all_of(c("file", "parent_ref", "a_ref", "annotation", "tier"))) |>
          tidyr::pivot_wider(names_from = dplyr::all_of("tier"), values_from = dplyr::all_of("annotation"), values_fill = "#E_M_P_T_Y#")
        
        # Pivot data into wide format, connecting child annotations with their parents
        annotations <-
          parent_annotations |>
          dplyr::left_join(child_annotations, by = dplyr::join_by("file" == "file",
                                                                  "a" == "a_ref",
                                                                  "tier" == "parent_ref")) |> 
          dplyr::mutate(parent_ref = .data$tier) |>
          dplyr::select(-dplyr::all_of(c("tier", "annotation"))) |>
          tidyr::pivot_longer(cols = dplyr::all_of(child_tiers),
                              names_to = "tier",
                              values_to = "annotation") |>
          dplyr::filter(.data$tier %in% child_tiers) |> 
          dplyr::filter(.data$annotation != "#E_M_P_T_Y#")
        
      }
      
      # Filter to only glosses specified in input
      if (length(glosses) > 0) {
        
        annotations <- 
          annotations |> 
          dplyr::filter(.data$annotation %in% glosses)
        
      }
      
      # Return annotations
      annotations
      
    }
    
  }
  
  # Read all files
  filenames |>
    purrr::map(read_eaf1, .progress = TRUE) |>
    purrr::list_rbind()
  
}

# ffmpeg_cut creates an ffmpeg command for cutting a video file by start/end times
ffmpeg_cut <- function(video_path, outfile, start, end, video_output_format = "", preset = "medium", crf = "12", padding = 0) {
  
  # String for ffmpeg command
  ffmpeg_cmd <- paste0("ffmpeg -i ", 
                       video_path, 
                       " -ss ",
                       format(as.POSIXct((start-padding) / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"), 
                       " -to ", 
                       format(as.POSIXct((end+padding) / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"),
                       paste0(" -preset ", preset, " -crf ", crf, " "),
                       outfile)
  
  # Execute the command
  system(ffmpeg_cmd)
  
}



# df2vid ------------------------------------------------------------------

# df2vid inputs a tibble (e.g. from eaf2df) and iterates through rows to cut video based on time codes
df2vid <- function(data, video_file_col = "video_file", video_path = "", destination = "", video_output_format = "", preset = "medium", crf = "12", padding = 0, tags = c("annotation"), sep = "_") {
  
  # Iterate through the data frame rows to process each sign to be extracted from the video
  for (n in 1:nrow(data)) {
    
    # Create a tag label to add to the filename (for unique identification)
    tag_label <- paste0(sep, paste0(data[n, tags], collapse = sep))
    
    # Use original video file format if none is provided explicitly
    if (video_output_format == "") {
      video_output_format <- paste0(".", tools::file_ext(data[n, video_file_col]))
    }
    
    # The video file to be processed is the path + the filename provided in df
    infile <- paste0(video_path, data[n, video_file_col])
    
    # Outfile is destination path plus infile + tags and output format
    outfile <- paste0(destination, basename(tools::file_path_sans_ext(data[n, video_file_col])), tag_label, video_output_format)
    
    # Stop if filename already exists (to avoid overwriting data)
    stopifnot("Error: Filename already exists! Add tags to make filenames unique." = !file.exists(outfile))
    
    # Run ffmpeg_cut with specified parameters
    ffmpeg_cut(video_path = infile, outfile = outfile, start = data[n, ]$start, end = data[n, ]$end, preset = preset, crf = crf, padding = padding)
    
  }
  
  
}



# eaf2vid -----------------------------------------------------------------

# eaf2vid inputs a directory of EAF (.eaf) files and cuts assumed associated video files based on segmentations
eaf2vid <- function(eaf_path, video_path, destination, tiers = c(), glosses = c(), recursive = FALSE, video_output_format = ".mp4", preset = "medium", crf = "12", padding = 0, tags = c("annotation", "start"), sep = "_") {
  
  eaf2df(eaf_path, tiers, glosses, recursive) |> 
    dplyr::mutate(video_file = paste0(tools::file_path_sans_ext(.data$file), video_output_format)) |> 
    df2vid(video_file_col = "video_file", video_path, destination, video_output_format, preset, crf, padding, tags, sep)
  
}



# Examples ----------------------------------------------------------------


# A simple usage case would be to simply direct the eaf2vid function to 
# a) a directory of ELAN files
# b) a directory of video files
# c) a directory for output videos
# and choose which tiers are to be used for finding segments, with the option
# to restrict the output only to certain target glosses.
# If there is a suffix to the video file (compared to the EAF file),
# this could be specified with the `video_output_format` argument
#
#eaf2vid(eaf_path = "/path/to/eafs/", 
#        video_path = "/path/to/vids/", 
#        destination = "/path/to/output-videos/",
#        tiers = c("sign-gloss"), 
#        glosses = c("PALMS-UP"), 
#        video_output_format = "_Signer_001.mp4")



# For more flexibility, it is advised to pre-process the data frame to be 
# used as input for the video processing, e.g. complex file name correspondances
# 
#eaf2df(path = "/path/to/eafs/", 
#       tiers = c("sign-gloss"), 
#       glosses = c("PALMS-UP")) |> 
#  dplyr::mutate(video_file = paste0(tools::file_path_sans_ext(file), "_", participant "_", ".mp4")) |> 
#  df2vid(video_path = "/path/to/vids/", 
#         destination = "/path/to/output-videos/", 
#         tags = c("annotation", "start"), # Tags used in the output filenames
#         video_file_col = "video_file", # Specifies the column to look for video filename
#         padding = 100 # Adds 100 milliseconds of padding to the video before and after the time stamps
#         )
