#!/bin/bash
# Charles Hawkins, 2022-08-01
#
# Run this from inside pgdbs/user/ or pass that directory as an argument. For
# each pgdb there, it will delete all versions that aren't the one listed in
# default-version. If the version in default-version is not found, no versions
# of that pgdb will be deleted since something is clearly not right. By default
# it only prints what would be deleted and kept (dry run); run it as
# delete-old-versions.sh -d [pgdbs_dir] to actually delete the old versions


# Parse flags and set $really_delete if we get -d; we want to really do the deletion if $really_delete is true 
really_delete=false
while getopts "d" flag; do
	case "$flag" in
	    d) really_delete=true;;
	esac
done

# Remaining arg after the flags, if present, is the directory argument
dir=${@:$OPTIND:1}
if [[ -n $dir ]]; then
	cd $dir
fi

# Loop over pgdb subdirectories in the directory
for pgdb in $(ls); do
	# Skip non-directories
	if [[ ! -d $pgdb ]]; then continue; fi
	cd $pgdb
	if [[ -a default-version ]]; then
		# Read the default version (the one we will keep) from the default-version file into $version_to_keep
		version_to_keep=$(cat default-version)
		# Make sure the version referred to in default-version is actually present; if not then something is wrong and we shouldn't delete anything
		if [[ -a $version_to_keep ]]; then
			# Loop over the versions that are present in the folder; $version is the version we're considering deleting
			for version in $(ls); do
				# Skip over non-directories again
				if [[ ! -d $version ]]; then continue; fi
				if [[ $version == $version_to_keep ]]; then
					if $really_delete; then
						echo "keeping $pgdb $version"
					else
						echo "would keep $pgdb $version"
					fi
				else
					if $really_delete; then
						echo "deleting $pgdb $version"
						rm -rf $version
					else
						echo "would delete $pgdb $version"
					fi
				fi
			done
		else
			echo "$pgdb: default version $version_to_keep not found, will not delete any versions"
		fi
	else
		echo "$pgdb: no default-version file found, skipping"
	fi
	cd ..
done
if ! $really_delete; then
	echo "This was a dry run; to really delete the old versions, run with -d (before the directory name)"
fi
