# A list of terms and translations we can use in Janus data
# Ultimately these should derive from some central source/s
# But some may be locally hardcoded in the meanwhile.
# bmh oct 2011

# USAGE
# *note - assumes you have loaded 'data' object that fits this ontology
# source('../data/ontology.R')
# o <- ontology$load(data)
# data[o$NEUTRON,o$MACROS]
# 
# ...there are many other selectors shown below

# @TODO Get these from a remote source

# A namespace
ontology <- list()

ontology$load <- function(data){
	o <- list()
    #column cateogories
	o$MACROS <- c("NTYG_L", "PNC_N", "TADN_N", "TADN_L", "BDY_N", "CDU_L", "CLR_N", "TOVE_N", "HNP_N", "THGL_N", "PNU_N", "CYS_N", "ANE_N", "EDA_N", "NTYG_N", "HTX_N", "HRG_N", "TSEC_N", "MKY_N", "TVAS_L", "TLIV_N", "ANE_L", "DIV_N", "PNU_L", "CRD_N", "TVAS_N", "OVE_N", "TADR_N", "TLIV_L", "TPYL_N", "LIV_N", "TCON_L", "HGL_N", "ASC_N", "TOVE_L", "MGL_N", "ADH_N", "HRT_N", "TTYG_L", "HEP_N", "PNC_L", "TUTE_N", "ENT_N", "ADR_N", "NTYL_L", "HRG_L", "UTE_N", "NTYL_N", "ENT_L", "DIV_L", "TCON_N", "TTYL_L", "TKID_N", "BSC_N", "THGL_L", "TKID_L", "DER_N", "SPL_N", "KID_N", "THR_N", "TBON_N", "TEP_N", "TBON_L", "TPIT_N", "ACI_N", "HNP_L", "PAR_N", "TSTO_N", "BLA_N", "OBS_N", "TMUS_N", "HEP_L", "TMUS_L", "TRD_N", "BRN_N", "PIT_N", "TMGL_N", "MIR_N", "TADR_L", "CRD_L", "MIG_N", "GON_N", "FIT_L", "TMGL_L", "TPIT_L", "TSKN_N", "GBL_N", "TUTE_L", "TSPL_N", "CYS_L", "PCK_N", "MET_N", "THR_L", "TSPL_L", "ACI_L", "HEM_N", "TWI_N", "PGL_N", "TTST_N", "TPYL_L", "CNS_N", "STO_N", "TMIL_L", "LIV_L", "TTRD_N", "MIR_L", "TMIC_L", "PAN_N", "JAU_N", "TSTO_L", "SGL_N", "MIG_L", "SEM_N", "TISO_N", "ISO_N", "TTST_L", "PRO_N", "NEC_N", "TJEJ_L", "TTYL_N", "TMIC_N", "ULC_N", "BON_N", "FIT_N", "TTYG_N", "TBRN_L", "TSGL_L", "TSGL_N", "CAT_N", "BDY_L", "JEJ_N", "HTX_L", "TMIG_L", "ABS_N", "TMIL_N", "TJEJ_N", "OBS_L", "TSKN_L", "PRF_N", "PER_L", "PST_N", "BAC_N", "TYP_N", "INF_N", "CGL_N", "THRT_L", "BAC_L", "TDUO_N", "CAE_N", "BRN_L", "TTRD_L", "TEPI_N", "TCOL_L", "CNS_L", "TMIG_N", "HRT_L", "INT_N", "TILE_L", "TBLA_L", "PCD_N", "MID_L", "TYP_L", "TISO_L", "TPAN_N", "TSMV_N", "CIR_N", "TILE_N", "COL_N", "MET_L", "MID_N", "EMP_N", "TCEC_L", "PCD_L", "DHY_N", "THRT_N", "OBE_N", "TCOL_N", "TADP_N", "PER_N", "TPST_L", "MSC_N", "TMUG_L", "MAL_N", "CIR_L", "MIC_N", "MIS_N", "TPAN_L", "TSMV_L", "TCEC_N", "TMUG_N", "DER_L", "TPST_N", "MAL_L", "TSEC_L", "TDUO_L", "NEC_L", "TBLA_N", "INF_L", "SPL_L", "MIL_N", "TCNS_L", "TPPT_N", "BLA_L", "MIS_L", "HEM_L", "ANU_N", "TVAG_N", "ABS_L", "TMIR_L", "TBRN_N", "MIC_L", "ULC_L", "CLI_N", "TCNS_N", "UTE_L", "MKY_L", "ESO_N", "ILE_N", "PEN_N", "PRO_L", "TMIN_L", "TEPI_L", "TMIR_N", "TVAG_L", "PCK_L", "TMIS_N", "PRF_L", "INT_L", "TMID_N", "TMIN_N", "ASC_L", "KID_L", "OVE_L", "TMID_L", "VOL_L", "CAE_L", "TMIS_L", "ADH_L", "CAL_N", "VAG_N", "TCGL_N", "DUO_N", "COL_L", "THIB_N", "TCGL_L", "AMY_N", "THIB_L", "EPL_N", "MIL_L", "ILE_L", "CHO_N", "CLR_L", "GBL_L", "TTGE_N", "TADP_L", "VOL_N", "TESO_L", "STO_L", "PAR_L", "JEJ_L", "PAN_L", "JAU_L", "TGBL_N", "MGC_N", "CHO_L", "MGC_L", "TESO_N", "CGL_L", "TWI_L", "MYO_N", "DUO_L", "SEM_L", "TPPT_L", "PIT_L", "MYO_L", "TTGE_L", "EDA_L", "ESO_L", "CLI_L", "TGBL_L", "TGE_N", "EMP_L", "_L", "PST_L", "BON_L", "MSC_L", "EMB_N", "ANU_L", "GRY_N", "SGL_L", "TEP_L")
	o$RADIATION_TYPE = "radn"
	o$FRACTIONS = "fractions"
	o$cGY_PER_MINUTE = "dose_rate"
	o$cGY_TOTAL = "total_dose"

    #types of radiation
	o$NEUTRON <- (data["radn"] == "N")
	o$GAMMA <- (data["radn"] == "G")
	o$CONTROL <- (data["radn"] == "C")

	# Doses
	o$cGY_0        <- (data["total_dose"] == 0)
	o$cGY_2.05     <- (data["total_dose"] == 2.05) 
	o$cGY_7.69     <- (data["total_dose"] == 7.69)
	o$cGY_13.85    <- (data["total_dose"] == 13.85)
	o$cGY_21.54    <- (data["total_dose"] == 21.54)
	o$cGY_30.78    <- (data["total_dose"] == 30.78)
	o$cGY_37.68    <- (data["total_dose"] == 37.68)
	o$cGY_40.04    <- (data["total_dose"] == 40.04)
	o$cGY_100      <- (data["total_dose"] == 100)
	o$cGY_150.72   <- (data["total_dose"] == 150.72) 
	o$cGY_200      <- (data["total_dose"] == 200)
	o$cGY_300      <- (data["total_dose"] == 300)
	o$cGY_399.99   <- (data["total_dose"] == 399.99)
	o$cGY_450      <- (data["total_dose"] == 450)
	o$cGY_600      <- (data["total_dose"] == 600)
	o$cGY_1839.4   <- (data["total_dose"] == 1839.4)

    #dosing fractions
	o$FRACTIONS_60 <- (data["fractions"] == 60)

	# Autopsy results
	o$MACRO_COUNT <- rowSums(data[o$MACROS])
	o$HAS_MACRO <- (o$MACRO_COUNT != 0)
	return(o)
}
