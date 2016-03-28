README
======

Getting data from UC Davis into csv form ([details here][1]).

## Steps

1. **Get images.** Use `get_images.bash` to retrieve the front page of every pdf that contains "D00" in [the ucdavis archive][2]. I doubled checked and all were retrieved as expected and are now [in Dropbox][3].
2. **Remove shoddy images.** Organized those images into good, bad, and multipage folders. Good (~800) can be entered in mturk. Bad (~15) cannot be and will need tedious work to get. Multipage (~400) are actually just additional pages that belong to "good". They usually start with " #2" or " #3" and should be listed in references.
3. Load these images into a csv. `ucdavis_mturk_input.csv`.


TODO: 
	Add new groups and studies
	Run the big one
	Clean up the Bads
	Link to pdf (including multipages)
	clean with ucdavis_proofing_script.Rmd

[1]: https://dl.dropboxusercontent.com/u/1131693/ucdavis_proposal.pdf
[2]: http://janus.northwestern.edu/ucdavis/
[3]: https://www.dropbox.com/personal/Public/ucdavis

First create the morgify script
	Start at 


September 21ish

	~/Downloads/input.csv
	~/Downloads/Batch_2097329_batch_results.csv
	bloodrop - D05M64%20%231.png
	bloodrop - D05M09.png


Looks like that's all I've got. I will have to recreate the script to generate these images.
First do a few hundred then proof and set defaults on the form...


