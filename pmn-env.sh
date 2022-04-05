#!/bin/bash
# Source this file to set PMN-related variables. You should first set the $*_OV variables (listed below) if anything isn't in its default place. Tested to work with bash and zsh. This file should be in the base directory of the PMN pipeline. Works with bash and zsh. 

# Shell detection
if [[ -n $PMN_SHELL_OV ]]; then
	PMN_SHELL=$PMN_SHELL_OV
else
	shell_str=$(ps -p $$ | tail -1 | awk '{print $4}')
	if [[ $shell_str == *bash* ]]; then
		export PMN_SHELL=bash
	elif [[ $shell_str == *zsh* ]]; then
		export PMN_SHELL=zsh
	else
		# This error message admittedly usually doesn't get printed as most shells won't recognize the above if statements
		echo "Unsupported shell: $shell_str. The pipeline supports only bash and zsh. If detaction has failed "
		return 1
	fi
fi
echo "Shell is $PMN_SHELL"

# OS detection (i.e. whether we're using GNU or BSD tools)
un=$(uname)
if [[ -n $PMN_OS_OV ]]; then
	if [[ $PMN_OS_OV == GNU || $PMN_OS_OV == BSD ]]; then
		export PMN_OS=$PMN_OS_OV
	else
		echo "Unrecognized value of PMN_OS_OV: \"$PMN_OS_OV\". Please set it to \"GNU\" (most Linux distros), \"BSD\" (macOS and most BSDs), or leave unset (will attempt to autodetect), and re-source $0"
		return 1
	fi
elif [[ $un == *Linux* || $un == *GNU* ]]; then
	PMN_OS=GNU
elif [[ $un == Darwin || $un == *BSD* || $un == DragonFly ]]; then
	PMN_OS=BSD
else
	echo "Did not recognize os \"$un\". Please manually set PMN_OS_OV to \"GNU\" or \"BSD\" and re-source $0"
	return 1
fi
echo "Environment is $PMN_OS"

# Detect if the script is being executed rather than sourced; that won't work, so tell the user and exit
if [[ $PMN_SHELL == bash ]]; then
	if ! (return 0 2>/dev/null); then
		echo $0 should be sourced, not run as an executable. I.e. you should execute \"source $0\"
		exit 1
	fi
else #zsh
	if [[ ! $ZSH_EVAL_CONTEXT == *file ]]; then
		echo $0 should be sourced, not run as an executable. I.e. you should execute \"source $0\"
		exit 1
	fi
fi

if [[ $PMN_SHELL == bash ]]; then
	pmn_env_src=$BASH_SOURCE
else #zsh
	pmn_env_src=$0
fi
export PMN_PGP=$(cd $(dirname $pmn_env_src); pwd -P)
echo "Pipeline directory is $PMN_PGP"
export PMN_BIN=$PMN_PGP/bin
if [[ -z $PMN_OLDPATH ]]; then
	PMN_OLDPATH=$PATH
fi
export PATH=$PMN_OLDPATH:$PMN_BIN
export PMN_PERL_SCRIPTS=$PMN_PGP/perl_scripts
export PMN_PERL_MODS=$PMN_PGP/perl_modules
export PMN_E2P2=${PMN_E2P2_OV:-$PMN_PGP/E2P2}
if [[ ! -a $PMN_E2P2 ]]; then
	echo "Warning: E2P2 directory $PMN_E2P2 does not exist; please download E2P2 into $PMN_E2P2 from https://github.com/carnegie/E2P2 or specify an existing install by setting PMN_E2P2_OV"
fi
export PMN_E2P2_RPSD=${PMN_E2P2_RPSD_OV:-4.2}
export PMN_LISP=$PMN_PGP/lisp
export PMN_LISP_FUNS=$PMN_LISP/pmn-lisp-funs.lisp
export PMN_BUSCO=${PMN_BUSCO_OV:-$PMN_PGP/busco}
export PMN_BUSCO_DB=${PMN_BUSCO_DB_OV:-eukaryota_odb10}
if [[ ! -a $PMN_BUSCO/$PMN_BUSCO_DB ]]; then
	echo "Warning: BUSCO database $PMN_BUSCO/$PMN_BUSCO_DB does not exist; please obtain $PMN_BUSCO_DB from https://busco-data.ezlab.org/v4/data/lineages/ and place it in $PMN_BUSCO, or specify an alternate location and database name by setting PMN_BUSCO_OV and PMN_BUSCO_DB_OV"
fi

# SAVI-related variables
export PMN_SAVI=${PMN_SAVI_OV:-$PMN_PGP/savi}
export PMN_SAVI_FILES=(AIPP.txt CAPP.txt UPP.txt CVP.txt NPP.txt)

#Pathwy Tools-related variables
export PT_APP=${PT_APP_OV:-~/pathway-tools}
echo "Using $PT_APP as Pathway Tools executable directory (to override, set PT_APP_OV)"
if [[ ! -a $PT_APP ]]; then
	echo "Warning: Pathway Tools executable directory $PT_APP does not exist; please install Pathway Tools (http://bioinformatics.ai.sri.com/ptools/) or specify the pathway-tools directory of an existing install by setting PT_APP_OV"
	return 1
fi
export PT_LOC=${PT_LOC_OV:-~/ptools-local}
echo "Using $PT_LOC as Pathway Tools executable directory (to override, set PT_LOC_OV)"
if [[ ! -a $PT_APP ]]; then
	echo "Warning: Pathway Tools data directory $PT_LOC does not exist; please install Pathway Tools (http://bioinformatics.ai.sri.com/ptools/) or specify the ptools-local directory of an existing install by setting PT_LOC_OV"
	return 1
fi
if [[ -n $PT_VER_OV ]]; then
	PT_VER=$PT_VER_OV
	echo "Pathway Tools version manually set to $PT_VER"
elif [[ -a $PT_APP/aic-export/pathway-tools/ptools ]]; then
	PT_VER=$(command ls $PT_APP/aic-export/pathway-tools/ptools | tail -1)
	echo "Pathway Tools version detected as $PT_VER"
fi
export PT_EXE=$PT_APP/aic-export/pathway-tools/ptools/$PT_VER/pathway-tools
export PMN_PGDBS=/$PT_LOC/pgdbs/user
export PMN_METACYC=$PT_APP/aic-export/pgdbs/metacyc
if [[ -d $PMN_PGDBS/plantcyc ]]; then
	export PMN_PLANTCYC=$PMN_PGDBS/plantcyc
	echo "Plantcyc found at $PMN_PLANTCYC"
else
	echo "Plantcyc not found; if you want to import data from plantcyc into your PGDB(s), please download plantcyc from PMN (the license form to be able to download PMN databases is at https://plantcyc.org/downloads/license-agreement) and install it in $PMN_PGDBS/plantcyc "
fi
