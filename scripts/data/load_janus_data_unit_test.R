# A unit test
# Can be run directly
# Also can be used as a reference for janus values
# bmh Oct 2011
#
# @TODO left off with the mock treated field, strangely this has NA values 
#       where the other logicals dont
# @TODO migrate the set of table and summary generators here into a general 
#		summary function that can be used on any sub or super set of radiobiology 
#		data

# Basic Usage
# Load a fresh copy of the data
source('../data/load_janus_data.R')
data <- j.data$load(from_cache=TRUE)

# CACHE TEST - slow
# The cache shoult be as good as loading from source
stopifnot(data == j.data$load(from_cache=FALSE))
# Clear memory to prevent a logjam
gc()

# Tests
# Check by ensuriring that the head looks like we expect
# including some sample data, column names, and dimensions
# and that the types are as we expect
na.dates <- 9741
stopifnot(na.dates == sum(is.na(data[["necroscopy_date"]])))

summary.dates <- c(
	"Min."="1971-09-30",
	"1st Qu."="1976-07-19",
	"Median"="1979-03-07",
	"Mean"="1979-09-03",
	"3rd Qu."="1983-03-14",
	"Max."="1989-07-28" 
	)
stopifnot(summary.dates == as.character(summary(data[["necroscopy_date"]])))   

table.proctor <- c(
	9741,
	"AL"=649,
	"AS"=11016,
	"BF"=592,
	"BJ"=5633,
	"CZ"=1,
	"ES"=8228,
	"FG"=97,
	"JH"=1222,
	"JP"=126,
	"JR"=142,
	"KA"=11391,
	"MF"=1145,
	"PD"=127
	)
stopifnot(table.proctor == table(data[["necrosopy_proctor"]]))

na.age <- 0
stopifnot(na.age == sum(is.na(data[["age"]])))

summary.ages <- c(
	"Min."="91",
	"1st Qu."="653",
	"Median"="862",
	"Mean"="832.1",
	"3rd Qu."="1024",
	"Max."="2367"
	)
stopifnot(summary.ages == as.character(summary(data[["age"]])))

na.age <- 0
stopifnot(na.age == sum(is.na(data[["age"]])))

summary.ages <- c(
	"Min."="91",
	"1st Qu."="653",
	"Median"="862",
	"Mean"="832.1",
	"3rd Qu."="1024",
	"Max."="2367"
	)
stopifnot(summary.ages == as.character(summary(data[["age"]])))

na.sex <- 0
stopifnot(na.sex == sum(is.na(data[["sex"]])))

table.sex <- c(
	"F"=23460,
	"M"=26650
	)
stopifnot(table.sex == table(data[["sex"]]))

table.species <- c(
	885,
	"Mus musculus"=46835,
	"Peromyscus leucopus"=2390
	)
stopifnot(table.species == table(data[["species"]]))

table.expt <- c(
	"2"=11585,
	"3"=3275,
	"4"=9415,
	"7"=2735,
	"8"=1880,
	"9"=5450,
	"10"=2390,
	"11"=885,
	"12"=600,
	"13"=7895,
	"14"=4000 
	)
stopifnot(table.expt == table(data[["expt"]]))

table.tmt <- c(
	"00"=510,
	"0p"=400,
	"0s"=400,
	"0x"=1410,
	"1x"=2400,
	"2x"=1105,
	"3x"=875,
	"4x"=1070,
	"5x"=670,
	"6x"=365,
	"a0"=400,
	"ac"=400,
	"ai"=800,
	"ap"=400,
	"ar"=400,
	"bi"=800,
	"bp"=400,
	"br"=400,
	"c0"=400,
	"cp"=400,
	"dc"=400,
	"di"=800,
	"dp"=400,
	"e1"=465,
	"e2"=220,
	"e3"=150,
	"e4"=50,
	"ec"=400,
	"ei"=800,
	"hc"=400,
	"hi"=800,
	"j0"=120,
	"j1"=120,
	"j2"=120,
	"j4"=120,
	"j6"=120,
	"k0"=480,
	"k1"=2070,
	"k2"=1410,
	"k3"=670,
	"k4"=540,
	"k5"=560,
	"k6"=340,
	"l0"=200,
	"l1"=200,
	"l2"=100,
	"l3"=80,
	"l4"=40,
	"l5"=175,
	"l6"=100,
	"l7"=75,
	"lc"=175,
	"q1"=345,
	"q2"=760,
	"r1"=400,
	"r2"=720,
	"s0"=795,
	"s1"=1600,
	"s2"=800,
	"s3"=800,
	"s4"=900,
	"s5"=520,
	"s6"=520,
	"s7"=360,
	"s8"=480,
	"sh"=50,
	"sl"=50,
	"u0"=190,
	"u1"=880,
	"u2"=440,
	"u3"=370,
	"v0"=245,
	"v1"=400,
	"v2"=400,
	"v3"=350,
	"v4"=320,
	"vv"=250,
	"vw"=215,
	"w0"=610,
	"w1"=850,
	"w2"=950,
	"x0"=400,
	"x1"=500,
	"x2"=650,
	"x3"=600,
	"x4"=750,
	"x5"=450,
	"x6"=350,
	"x7"=250,
	"x8"=200,
	"x9"=150,
	"xc"=750,
	"xx"=400,
	"y2"=600,
	"y3"=600,
	"z2"=595,
	"z3"=590 
)
stopifnot(table.tmt == table(data[["tmt"]]))

summary.first_irrad <- c(
	"Min."="93",
	"1st Qu."="108",
	"Median"="114",
	"Mean"="128.3",
	"3rd Qu."="117",
	"Max."="519"
	)
stopifnot(summary.first_irrad == as.character(summary(data[["first_irrad"]])))

summary.total_dose <- c(
	"Min."="0",
	"1st Qu."="2.05",
	"Median"="43.15",
	"Mean"="243.7",
	"3rd Qu."="226.1",
	"Max."="4901"
	)
stopifnot(summary.first_irrad == as.character(summary(data[["first_irrad"]])))

table.radn <- c(
	"C"=8465,
	"G"=18495,
	"N"=23150
	)
stopifnot(table.radn == table(data[["radn"]]))

