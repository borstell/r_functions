# Repo with random R functions

## ELAN functions

### `read_eaf()`
Function based on original from the [`signglossR`](https://github.com/borstell/signglossR) package. Reads either all ELAN files (`.eaf`) from a directory (if path is set to a directory) or an individual file (if path is set to a file). Returns a tibble dataframe with all the data.

**Example:**
```
# Read all the EAF files of the directory into a single tibble dataframe
all_eafs <- read_eaf(path="/Users/username/Desktop/Corpus/EAFs/")

# Read an individual EAF file into a single tibble dataframe
my_eaf <- read_eaf("/Users/username/Desktop/Corpus/EAFs/FrogStory_2022-12-01.eaf")
```


### `split_elan_videos()`
Uses the `read_eaf()` function to first read an ELAN file (or all ELAN files in a directory) and then split one (if single file path) or more (if entire directory) video files into clips corresponding to the segments on a tier in the ELAN file. The tier used for segmentation needs to be provided. Default input and output video file format is `.mp4` but can be specified with the arguments. Padding can be added with positive or negative values (in milliseconds) adding/removing frames at the beginning and end of the individual clips relative to the cell segment durations.

**Example:**
```
# Split all videos with file names mirroring the EAF file names
split_elan_video(elan_path = "/Users/username/Desktop/Corpus/EAFs/",
                 segmentation_tier = "name_of_segmentation_tier",
                 video_path = "/Users/username/Desktop/Corpus/videos/",
                 annotation_tag = T, # will add contents of ELAN cells in output filenames
                 padding = 0, # adds (or subtracts if negative) frames (in milliseconds) before+after segment duration
                 video_input_format = ".mov", # specify input video format in directory (default is .mp4)
                 video_output_format = ".mp4") # specify output video format (default is .mp4)

# Split a specific video based on an EAF file with a different name, adding 100 milliseconds before and after each segment
split_elan_video(elan_path = "/Users/username/Desktop/Corpus/EAFs/FrogStory_2022-12-01.eaf",
                 segmentation_tier = "name_of_segmentation_tier",
                 video_path = "/Users/username/Desktop/Corpus/videos/FrogStory_Signer012.mp4",
                 annotation_tag = T, # will add contents of ELAN cells in output filenames
                 padding = 100) # adds (or subtracts if negative) frames (in milliseconds) before+after segment duration
```
