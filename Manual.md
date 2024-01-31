# Manual for running the PMN pipeline
This pipeline is used by the Plant Metabolic Network (PMN) to produce the plant metabolism databases hosted at https://plantcyc.org. It can be run to produce similar databases for plant and green algal species. 

## Overview of the pipeline
The PMN pipeline starts with the amino acid sequences for a plant or green algal species and produces as its output a pathway-genome database (PGDB) containing a full predicted metabolism for the species. The databases are accessed using the Pathway Tools software from SRI International and can be hosted on the web using that software. Databases like this can serve as a general reference for the species metabolism, and can perform functions such as mapping between sets of compounds and sets of enzymes or pathways, or finding pathways enriched for a set of genes or metabolites.

To run the pipeline you need, at minimum, the amino acid sequences for your species of interest. If possible you should also have some means to map the protein IDs to their corresponding gene IDs. The pipeline presently requires downloaded copies of PlantCyc and AraCyc; these you can get by filling out a license request at (url); these licenses are free for all users. A copy of Pathway Tools is also required. Information about Pathway Tools can be found at (http://bioinformatics.ai.sri.com/ptools); as of January 2024, SRI offers free licenses to academic and government users, but commercial users will need a paid license to use their software.

The pipeline is built to run in a Linux environment on an x86_64 CPU. On Windows we recommend the Windows Subsystem for Linux (WSL) software from Microsoft (https://learn.microsoft.com/en-us/windows/wsl/install) that creates a Linux environment on Windows. As for macOS, we recommend a virtualization solution, such as Qemu, VirtualBox, VMWare, or Parallels, running a Linux guest. It may be possible to make the pipeline run natively on macOS but this has not been tested at all. It is unlikely the pipeline can be made to run on ARM CPUs due to the dependency on Pathway Tools.

The major pipeline stages are:

- **E2P2**, which predicts enzymes from protein sequences
- **PathoLogic**, which generates an initial pathway genome database from the predicted enzymes
- **SAVI**, which applies past curation decisions to improve the database accuracy at the pathway level
- **Refine-A**, which applies SAVI's recommended changes and does other fixes such as reaction names
- **Refine-B**, which pulls experimental evidence from the reference database for the species of interest
- **Refine-C**, which performs minor finishing actions such as generating the cellular overview diagram
- **Blastset generation**, an optional step that generates a blast database for the species so that protein queries can be used to find enzymes in the database.

Running the pipeline requires a basic familiarity with the Linux command line.

## Downloading and building the Singularity container
The recommended way to run the PMN pipeline is via the Singularity container. The container build scripts will handle installing all needed dependencies and putting them where the pipeline can find them. The only component you need to provide is a copy of the Pathway Tools installer. It is also possible to use a local copy of Pathway Tools that is already installed outside of the container; see "Use a local copy of Pathway Tools" below.

### Download the container build scripts
The container build scripts are available at the Github project (url). You may download and unzip one of the releases or git-clone the repository.
You will also need to install one of the two currently-active Singularity forks, SingularityCE (https://sylabs.io/singularity) or Apptainer (https://apptainer.org), to build the container.

### Obtain Pathway Tools
The pipeline relies on the Pathway Tools software from SRI Internaitonal. Because Pathway Tools is proprietary software, we cannot redistribute it with the pipeline and the container build scripts will not download it for you. Instead, you will need to supply the installer provided by SRI for installation into your copy of the pipeline container image. You can obtain a license from SRI International at (http://bioinformatics.ai.sri.com/ptools); as of January 2024 SRI offers free licenses to academic, government, and non-profit users, while commercial users will need a paid license. You will want to download the standard Linux installer, with a name like pathway-tools-27.0-linux-64-tier1-install, and place it in the directory with the container build scripts. 

#### Pathway Tools versions
Each release of the PMN pipeline is coded to use a particular version of Pathway Tools (see the release notes of each release), and this is hardcoded into the build scripts; if you want to use a different version instead (which will most likely work although the results may not be exactly comparable to the PMN databases built using that release of the pipeline), you should edit the file pmn-ptools.def and change the PT_VER variable and the name of the pathway tools installer further down the file to reflect the Pathway Tools version you want to use.

### Build the container
#### Apptainer vs SingularityCE
Currently, the Singularity project has two major forks, SingularityCE and Apptainer. At present there are no major differences between them that affect the PMN pipeline, so either will work as well as the other. Most Linux distributions should have one or both available in their package repositories.

If you are using Apptainer, you will need to set the environment variable `SINGULARITY=apptainer` for the build. If using SingularityCE, the default value will work.

#### Tempfs and disk space
Singularity by default uses the /tmp directory for all container builds. Somewhere around 16-18GB of space are needed there for the PMN container to build. On some Linux distributions, /tmp is a normal directory and there will be no problem as long as you have the disk space; however others such as Fedora use tmpfs and default /tmp to a size equivalent to half of your installed physical RAM, which may not be enough. You can check the space available with `df -h /tmp`. If there isn't enough space, or you encounter out of disk space errors during the build, you can tell Singularity to use another directory for its temporary files. To do this, do the following:

1. Create a tmp directory inside the PMN container build directory (mkdir tmp)
2. Specify the environment variable `TMPDIR=$PWD/tmp` for the build

You could also remount tmpfs with a larger size. Either should resolve the issue.

#### Build the PMN pipeline
Navigate to the build directory and type:

    make pmn-ptools.sif

(preceede it with any environment variable declarations you need from the previous sections). Assuming the build succeeds, you will have a finished container, pmn-ptools.sif. As the message at the end of the build states, this contains an installed copy of Pathway Tools, so please do not redistribute it without permission from SRI International. The build will also generate pmn.sif, which is a copy of the pipeline that does not contain Pathway Tools. This container must be used with an external installation of Pathway Tools but is safe to redistribute. If you don't need pmn.sif it is safe to delete after the build is finished.
