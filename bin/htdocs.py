#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
from os import path
import re

def main():
    par = ap.ArgumentParser(description = 'Generates htdocs for PMN website')
    pmn.add_standard_args(par)

    args = par.parse_args()
    (config, orgtable, orglist) = pmn.read_pipeline_files(args)
    generate_htdocs(config, orgtable, orglist)

if __name__ == '__main__':
    exit(main())

def generate_htdocs(config, orgtable, orglist):
    proj_htdocs = config['proj-htdocs-dir']
    ptools_htdocs = path.join(path.dirname(config['ptools-exe']), 'aic-export', 'htdocs')
    pmn.info(f'Using files in {ptools_htdocs} to generate htdocs in {proj_htdocs}')

    # Add PMN banner to template-after-beginning-body
    tabb ='template-after-beginning-body.shtml'
    tabb_out = open(path.join(proj_htdocs, tabb), 'w')
    steps_required = {'bannermenu', 'logolink'}
    steps_done = set()
    for line in open(path.join(ptools_htdocs, tabb)):
        if line.find('<div id="topBannerMenu">') > -1:
            line = line.replace('>', 'style="position:relative;z-index:10000">')
            pmn.info('Found topBannerMenu')
            steps_done.add('bannermenu')
        elif line.find('<a id="logoLink" class="logo" href="/"></a>') > -1:
            line = line.replace('>', 'class="logo" href="http://www.plantcyc.org">', 1)
            pmn.info('Found logoLink')
            steps_done.add('logolink')
        tabb_out.write(line)
    pmn.info(f'Steps done: {pmn.andlist(steps_done)}')
    for not_done in steps_required - steps_done:
        pmn.warn(f'Did not do step {not_done}')
    tabb_out.close()
    pmn.info(f'Finished writing {tabb}')

    # Add search keywords and google analytics tag to template-standard-head-content
    tshc = 'template-standard-head-content.shtml'
    tshc_out = open(path.join(proj_htdocs, tshc), 'w')
    tshc_out.write('<meta name="description" content="A collection of model organism databases of metabolic pathways, including reactions, enzymes, genes and substrate compounds">\n')
    tshc_out.write('<meta name="keywords" content="metabolism, pathways, metabolic, enzymes, compounds, reactions, biochemical, bioinformatics, model organism database, genes, genome, metabolic pathways, biochemical pathways, metabolic map, pathway database, metabolomics, omics, metabolic network, pathway viewer, metabolic pathway software, microarray data, gene expression, flux balance analysis, FBA, plant metabolism, view pathways">')

    tshc_out.write(open(path.join(ptools_htdocs, tshc)).read())
    tshc_out.write('''<script type="text/javascript">
<!--//--><![CDATA[//><!--
(function(i,s,o,g,r,a,m){i["GoogleAnalyticsObject"]=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]
).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.a
sync=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,"script","//www.google-analyti
cs.com/analytics.js","ga");ga("create", "UA-54274288-2", {"cookieDomain":"auto","allowLinker":
true});ga("require", "linker");ga("linker:autoLink", ["plantcyc.org","pmn.dpb.carnegiescience.
edu","pmn.dpb.ciw.edu","pmn-drupal.dpb.carnegiescience.edu","pmn-drupal.dpb.ciw.edu.edu"]);ga(
"set", "anonymizeIp", true);ga("send", "pageview");
//--><!]]>
</script>''')
    tshc_out.close()
    pmn.info(f'Wrote {tshc}')

    # Many things to add to template-top-menubar.shtml
    ttm = 'template-top-menubar.shtml'
    ttm_out = open(path.join(proj_htdocs, ttm), 'w')
    skiplines = 0
    del_endif = False
    skipuntil = None
    steps_required = {'home', 'databases', 'endif', 'show-blast', 'blast', 'species', 'genome', 'celov', 'help', 'growth', 'pattern', 'summary', 'history'}
    steps_done = set()
    ttm_in = open(path.join(ptools_htdocs, ttm))
    for line in ttm_in:
        # Set the 'home' menubar link to go to plantcyc.org
        if line.find('>Home<') > -1:
            pmn.info('Setting Home link to plantcyc.org')
            line = line.replace('href="/"', 'href="https://plantcyc.org"')
            steps_done.add('home')
        # Add the Databases menu just before the Search menu
        elif line.find('>Search<') > -1:
            pmn.info('Adding Databases menu')
            steps_done.add('databases')
            ttm_out.write('''<li class="yuimenubaritem"><a class="yuimenubaritemlabel" onclick="1">Databases</a>
     <div id="databases" class="yuimenu">

      <div class="bd">
        <ul class="first-of-type">\n''')
            ttm_out.write('<li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://pmn.plantcyc.org/organism-summary?object=PLANT">PlantCyc</a></li>\n')
            for org in orglist:
                ttm_out.write(f'<li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://pmn.plantcyc.org/organism-summary?object={org.upper()}">{org}Cyc</a></li>\n')
            ttm_out.write('''		</ul>
	  </div>
	 </div>
  </li>\n''')
        # Delete Search Growth Media (doesn't apply to plants), Sequence Pattern Search, Summary stats, History
        elif line.find('>Search Growth Media<') > -1:
            skiplines = 1
            steps_done.add('growth')
        elif line.find('>Sequence Pattern Search<') > -1:
            skiplines = 1
            steps_done.add('pattern')
        elif line.find('>Summary&nbsp;Statistics<') > -1:
            skiplines = 1
            steps_done.add('summary')
        elif line.find('>History&nbsp;of&nbsp;updates<') > -1:
            skiplines = 1
            steps_done.add('history')
        # Delete closing endif after show-blast-search-menu-item-p
        elif del_endif and line.find('#endif') > -1:
            skiplines = 1
            del_endif = False
            steps_done.add('endif')
        # Delete show-blast-search-menu-item-p condition, so we always show the BLAST menu item
        elif line.find('show-blast-search-menu-item-p') > -1:
            skiplines = 1
            del_endif = True
            steps_done.add('show-blast')
        # Modify BLAST menu item to link to PMN blast server
        elif line.find('blastMenuItem') > -1:
            steps_done.add('blast')
            line = '<li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://www.plantcyc.org/tools/Blast/blast.faces">BLAST at the PMN</a></li>'
        # After Google Search, add the Species in the PMN submenu (needs to go two lines after the Google Search line, hence the rigamarole)
        elif line.find('Google Search') > -1:
            pmn.info('Adding Species in the PMN')
            steps_done.add('species')
            ttm_out.write(line)
            ttm_out.write(ttm_in.readline())
            skiplines = 1
            ttm_out.write('''	<ul>
		<li class="yuimenuitem"><a class="yuimenuitemlabel" href="" onclick="false">Species&nbsp;in&nbsp;the&nbsp;PMN</a>
			<div id="species" class="yuimenu">
				<div class="bd">
					<ul class="first-of-type">
						<li class="yuimenuitem"><a class="yuimenuitemlabel" href="/PLANT/search-query?type=GENE&orgs=TAX-33090" target="_blank">Enzymes&nbsp;by&nbsp;plant&nbsp;species</li>
						<li class="yuimenuitem"><a class="yuimenuitemlabel" href="/PLANT/search-query?type=PATHWAY&orgs=TAX-33090" target="_blank">Pathways&nbsp;by&nbsp;plant&nbsp;species</a></li>
					</ul>         
				</div>
			</div>
		</li>
	</ul>
''')
        elif line.find('>Genome<') > -1:
            pmn.info('Found Genome, deleting all lines until Metabolism')
            steps_done.add('genome')
            skipuntil = '>Metabolism<'
        elif line.find('celOv.shtml') > -1:
            pmn.info('Adding Omics Viewer text to cel Ov menu item label')
            steps_done.add('celov')
            line = line.replace('Overview', 'Overview&nbsp;/&nbsp;Omics&nbsp;Viewer&nbsp;')
        elif line.find('>Contact&nbsp;Us') > -1:
            pmn.info('Adding help menu items')
            steps_done.add('help')
            line = '''	 <li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://www.plantcyc.org/help/faq">FAQ</a></li>
	 <li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://www.plantcyc.org/tutorials/tutorials-overview">Tutorials</a></li>
	 <li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://www.plantcyc.org/tools/omics-viewer-sample-data-files">Omics&nbsp;Viewer&nbsp;Sample&nbsp;Data</a></li>
	 <li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://www.plantcyc.org/feedback/feedback-form">Send&nbsp;Message</a></li>
	 <li class="yuimenuitem"><a class="yuimenuitemlabel" href="http://www.plantcyc.org/about/contact-information">Contact&nbsp;Info</a></li>
'''
        if skipuntil:
            if line.find(skipuntil) > -1:
                skipuntil = None
                ttm_out.write(line)
            else:
                pmn.info(f'Deleting line: {line.rstrip()}')
        elif skiplines:
            pmn.info(f'Deleting line: {line.rstrip()}')
            skiplines -= 1
        else:
            ttm_out.write(line)
    ttm_out.close()
    pmn.info(f'Steps done: {pmn.andlist(steps_done)}')
    for not_done in steps_required - steps_done:
        pmn.warn(f'Did not do step {not_done}')

