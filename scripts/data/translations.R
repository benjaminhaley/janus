# We need to load ways of translating janus data
# bmh Oct 2011

translations <- list()

translations <- list()

translations$load <- function(translations_sets, from_cache=FALSE){
	if(translations_sets == 'janus'){
		source('scripts/data/load_janus_translations.R')
		this.translations <- j.translations$load(from_cache)
	} else {
		stop("I do not recognize the translationsset entered")
	}
	return(this.translations)
}

