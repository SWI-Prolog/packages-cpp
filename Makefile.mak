################################################################
# Build the SWI-Prolog C++ interface for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk

all:		

!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		copy SWI-cpp.h "$(PLBASE)\include"
!ENDIF

xpce-install::

uninstall::
		del "$(PLBASE)\include\SWI-cpp.h"

html-install::
		copy pl2cpp.html "$(PKGDOC)"

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean


