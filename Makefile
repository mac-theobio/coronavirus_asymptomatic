
## This is https://github.com/mac-theobio/coronavirus_asymptomatic.git

current: target
-include target.mk

# -include makestuff/perl.def

######################################################################

# Content

Ignore += *.tgz

Sources += abs.tex acknowledge.tex appendix.tex asymp.tex author.tex biblio.tex settings.tex short.body.tex title.tex 
Sources += coronavirus.bib naturemag.bst

## asymp.pdf: asymp.tex
## asymp.bbl: coronavirus.bib

######################################################################

## Making a logical place for weitz_group code and figures
## Linking it temporarily to figures

Ignore += group
group:
	git clone https://github.com/WeitzGroup/coronavirus_asymptomatic.git $@

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls $@

-include makestuff/os.mk

-include makestuff/texdeps.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
