#	/home/astrand/GoogleDrive/unpak-database/database code/unpak-shiny/upload.test/upload-format/Makefile
#	$Modified:  Fri Feb 20 15:32:05 EST 2015$

#	  /usr/local/skel/Makefile.tex
#	  $Modified: Tue Oct 27 10:41:38 1998 by astrand $


#	  targets

PAPERDOCUMENTS =		upload-format.tex
SLIDEDOCUMENTS =		
DOCUMENTS =                     $(PAPERDOCUMENTS) $(SLIDEDOCUMENTS)
TEXTPAGES =		
FIGURES =		  
BIBFILES =		 

DVIFILES =		 $(DOCUMENTS:.tex=.dvi)
PSDOCS =		$(DOCUMENTS:.tex=.ps)
PDFDOCS =		  $(DOCUMENTS:.tex=.pdf)
PROOFS =		$(DOCUMENTS:.tex=.X)
PSPROOFS =		 $(DOCUMENTS:.tex=.Xps)
DRAFTS =		$(DOCUMENTS:.tex=.draft)
HTMLFILES =		$(DOCUMENTS:.tex=.html)
RCODENAME=		 $(PAPERDOCUMENTS:.tex=.R)
ODTNAME=		 $(DOCUMENTS:.tex=.odt)


#SLIDETEX=		$(DOCUMENTS:.tex=-slides.tex)
#SLIDEDVI=		$(DOCUMENTS:.tex=-slides.dvi)
#SLIDEPDF=		$(DOCUMENTS:.tex=-slides.pdf)
#SLIDEPS=		$(DOCUMENTS:.tex=-slides.ps)
#SLIDEBBL=		$(DOCUMENTS:.tex=-slides.bbl)
#SLIDEAUX=		$(DOCUMENTS:.tex=-slides.aux)


JUNK =		  *.aux *.[0-9]*.aux *.bbl *.[0-9]*.bbl *.blg \
			*.[0-9]*.blg *.dvi *.err *.glo *.idx *.lof \
			*.log *.lot *.toc *.tty *~ $(PSDOCS) body.tex\
			body-*.eps body-*.pdf *.tidx *.out

# uncomment  to reun the imagte conversion programs
# CONVERTIMAGE = YES

# add directories containing TeX input files
TEXINPUTS =		:.:

#add flags for Sweave and Stangle
SRCDEPOSIT =	    ../htdocs/source/

# add flags for latex2HTML
HTMLDIR =		  ../htdocs/latex2html
HTMLPRE =		  $(basename $(PSDOCS))
HTMLSPLIT =		5
HTMLFLAGS =		-info 0 -dir ${HTMLDIR} -prefix ${HTMLPRE} -split ${HTMLSPLIT}

include Makefile.knitr

tth:	
	tth upload-format.tex
	cp upload-format.html ../

# uncomment the following line to run BibTeX
upload-format.pdf: upload-format.tex upload-format.bbl
upload-format-slides.pdf: upload-format-slides.tex upload-format-slides.bbl





