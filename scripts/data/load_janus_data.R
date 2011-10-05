# To load the data from the janus studies. 
# Written by Ben Haley in March of 2011.
# Updated Oct 2011
# Usage:
#   See __FILE__unit_tests.R

# A namespace for data
#
j.data <- list()

# Dependencies 
#
source('../util/webcache.R')

# Configuration
#
j.data$.__URI <- "http://janus.northwestern.edu/janus2/data/"
j.data$.__DATA.DIR <- "../../data/"
j.data$.__CACHED.PATH <- "../../data/j.data.RDS"
j.data$.__TABLE.NAMES <- c("demographics", "macro_pathologies")
j.data$.__TABLE.HEADER.ROWS <- 1
j.data$.__ZIP.EXTENSION <- ".zip"
j.data$.__FILE.EXTENSION <- ".csv"

# @TODO replace this monstrocity with a function that loads the key and other translation features
j.data$.__MACROS <- c("NTYG_L", "PNC_N", "TADN_N", "TADN_L", "BDY_N", "CDU_L", "CLR_N", "TOVE_N", "HNP_N", "THGL_N", "PNU_N", "CYS_N", "ANE_N", "EDA_N", "NTYG_N", "HTX_N", "HRG_N", "TSEC_N", "MKY_N", "TVAS_L", "TLIV_N", "ANE_L", "DIV_N", "PNU_L", "CRD_N", "TVAS_N", "OVE_N", "TADR_N", "TLIV_L", "TPYL_N", "LIV_N", "TCON_L", "HGL_N", "ASC_N", "TOVE_L", "MGL_N", "ADH_N", "HRT_N", "TTYG_L", "HEP_N", "PNC_L", "TUTE_N", "ENT_N", "ADR_N", "NTYL_L", "HRG_L", "UTE_N", "NTYL_N", "ENT_L", "DIV_L", "TCON_N", "TTYL_L", "TKID_N", "BSC_N", "THGL_L", "TKID_L", "DER_N", "SPL_N", "KID_N", "THR_N", "TBON_N", "TEP_N", "TBON_L", "TPIT_N", "ACI_N", "HNP_L", "PAR_N", "TSTO_N", "BLA_N", "OBS_N", "TMUS_N", "HEP_L", "TMUS_L", "TRD_N", "BRN_N", "PIT_N", "TMGL_N", "MIR_N", "TADR_L", "CRD_L", "MIG_N", "GON_N", "FIT_L", "TMGL_L", "TPIT_L", "TSKN_N", "GBL_N", "TUTE_L", "TSPL_N", "CYS_L", "PCK_N", "MET_N", "THR_L", "TSPL_L", "ACI_L", "HEM_N", "TWI_N", "PGL_N", "TTST_N", "TPYL_L", "CNS_N", "STO_N", "TMIL_L", "LIV_L", "TTRD_N", "MIR_L", "TMIC_L", "PAN_N", "JAU_N", "TSTO_L", "SGL_N", "MIG_L", "SEM_N", "TISO_N", "ISO_N", "TTST_L", "PRO_N", "NEC_N", "TJEJ_L", "TTYL_N", "TMIC_N", "ULC_N", "BON_N", "FIT_N", "TTYG_N", "TBRN_L", "TSGL_L", "TSGL_N", "CAT_N", "BDY_L", "JEJ_N", "HTX_L", "TMIG_L", "ABS_N", "TMIL_N", "TJEJ_N", "OBS_L", "TSKN_L", "PRF_N", "PER_L", "PST_N", "BAC_N", "TYP_N", "INF_N", "CGL_N", "THRT_L", "BAC_L", "TDUO_N", "CAE_N", "BRN_L", "TTRD_L", "TEPI_N", "TCOL_L", "CNS_L", "TMIG_N", "HRT_L", "INT_N", "TILE_L", "TBLA_L", "PCD_N", "MID_L", "TYP_L", "TISO_L", "TPAN_N", "TSMV_N", "CIR_N", "TILE_N", "COL_N", "MET_L", "MID_N", "EMP_N", "TCEC_L", "PCD_L", "DHY_N", "THRT_N", "OBE_N", "TCOL_N", "TADP_N", "PER_N", "TPST_L", "MSC_N", "TMUG_L", "MAL_N", "CIR_L", "MIC_N", "MIS_N", "TPAN_L", "TSMV_L", "TCEC_N", "TMUG_N", "DER_L", "TPST_N", "MAL_L", "TSEC_L", "TDUO_L", "NEC_L", "TBLA_N", "INF_L", "SPL_L", "MIL_N", "TCNS_L", "TPPT_N", "BLA_L", "MIS_L", "HEM_L", "ANU_N", "TVAG_N", "ABS_L", "TMIR_L", "TBRN_N", "MIC_L", "ULC_L", "CLI_N", "TCNS_N", "UTE_L", "MKY_L", "ESO_N", "ILE_N", "PEN_N", "PRO_L", "TMIN_L", "TEPI_L", "TMIR_N", "TVAG_L", "PCK_L", "TMIS_N", "PRF_L", "INT_L", "TMID_N", "TMIN_N", "ASC_L", "KID_L", "OVE_L", "TMID_L", "VOL_L", "CAE_L", "TMIS_L", "ADH_L", "CAL_N", "VAG_N", "TCGL_N", "DUO_N", "COL_L", "THIB_N", "TCGL_L", "AMY_N", "THIB_L", "EPL_N", "MIL_L", "ILE_L", "CHO_N", "CLR_L", "GBL_L", "TTGE_N", "TADP_L", "VOL_N", "TESO_L", "STO_L", "PAR_L", "JEJ_L", "PAN_L", "JAU_L", "TGBL_N", "MGC_N", "CHO_L", "MGC_L", "TESO_N", "CGL_L", "TWI_L", "MYO_N", "DUO_L", "SEM_L", "TPPT_L", "PIT_L", "MYO_L", "TTGE_L", "EDA_L", "ESO_L", "CLI_L", "TGBL_L", "TGE_N", "EMP_L", "_L", "PST_L", "BON_L", "MSC_L", "EMB_N", "ANU_L", "GRY_N", "SGL_L", "TEP_L")

# Our primary function, return a data frame,
j.data$load <- function(from_cache=FALSE){
	if(from_cache==FALSE){
		uris <- j.data$.__get_uris()
		zip_paths <- j.data$.__download(uris)
		csv_paths <- j.data$.__unzip(zip_paths)
		raw_data <- j.data$.__csv2data.frame(csv_paths, j.data$.__TABLE.HEADER.ROWS)
		data <- j.data$.__normalize(raw_data)
		saveRDS(data, j.data$.__CACHED.PATH) 
	}
	data <- readRDS(j.data$.__CACHED.PATH)
	return(data)
}

# Convert types where needed
#
j.data$.__normalize <- function(raw_data){
	data <- raw_data
	names(data) <- j.data$.__get_normalized_names(names(data))
	data[['necroscopy_date']] <- as.Date(data[['necroscopy_date']], format="%Y-%m-%d" )
	data[['expt']] <- as.factor(data[['expt']])
	data[j.data$.__MACROS] <- data.frame(mapply(function(column){
		column[is.na(column)] <- c(FALSE)
		return(column)
	},data[j.data$.__MACROS]))
	return(data)
}

# Get the names from the mash of R
j.data$.__get_normalized_names <- function(names){
	period_split <- strsplit(names,".", fixed=TRUE)
	names <- mapply(function(period_split){
		last_elem <- period_split[length(period_split)]
		name <- last_elem
		return(name)
	},period_split)
	return(names)
}


# We need the full uri's for download
j.data$.__get_uris <- function(){
	uris = paste( 
			j.data$.__URI, 
			j.data$.__TABLE.NAMES, 
			j.data$.__ZIP.EXTENSION, 
			sep="" 
			)
	return(uris)
}

# Download w caching whoa!
j.data$.__download <- function(uris){
	local_paths <- mapply(cache$get, uris)
	return(local_paths)
}

# Unzip and keep the originals
j.data$.__unzip <- function(zip_paths){
	csv_paths <- mapply(function(zip_path){
		unzip(zip_path, exdir=j.data$.__DATA.DIR)
		name <- as.character(unzip(zip_path, list=TRUE)[1,1])
		csv_path <- paste(j.data$.__DATA.DIR, name, sep="")
		return(csv_path)
	}, zip_paths)
	return(csv_paths )
}

# Convert the data into R and delete the file
j.data$.__csv2data.frame <- function(paths, header_rows){
	data <- mapply( 
		read.csv, 
		paths, 
		as.is = TRUE,
		header = TRUE, 
		skip=1, 
		row.names = j.data$.__TABLE.HEADER.ROWS 
		)
	data <- data.frame(data)
	mapply( 
	   unlink, 
	   paths 
	)
	return(data)
}

		
