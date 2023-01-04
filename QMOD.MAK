PROJ = QMOD
PROJFILE = QMOD.MAK
DEBUG = 0

PWBRMAKE  = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake
BC  = bc
BCFLAGS_R  = /Ot
BCFLAGS_D  = /D /Zi
BCFLAGS_G  = /O /FPi /G2 /Lr /Fs /Ah
MAPFILE_D  = NUL
MAPFILE_R  = NUL
LFLAGS_G  = /NOI /BATCH
LFLAGS_D  = /CO /FAR /PACKC
LFLAGS_R  = /EXE /FAR /PACKC
LINKER	= link
ILINK  = ilink
LRF  = echo > NUL

OBJS  = QMOD.obj
SBRS  = QMOD.sbr

all: $(PROJ).exe

.SUFFIXES:
.SUFFIXES: .obj .sbr .bas

QMOD.obj : QMOD.BAS

QMOD.sbr : QMOD.BAS


$(PROJ).bsc : $(SBRS)
	$(PWBRMAKE) @<<
$(BRFLAGS) $(SBRS)
<<

$(PROJ).exe : $(OBJS)
!IF $(DEBUG)
	$(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_D)
$(LLIBS_G: = +^
) +
$(LLIBS_D: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_D);
<<
!ELSE
	$(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_R)
$(LLIBS_G: = +^
) +
$(LLIBS_R: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_R);
<<
!ENDIF
	$(LINKER) @$(PROJ).lrf


.bas.obj :
!IF $(DEBUG)
	$(BC) /Z $(BCFLAGS_G) $(BCFLAGS_D) $<, $@;
!ELSE
	$(BC) /Z $(BCFLAGS_G) $(BCFLAGS_R) $<, $@;
!ENDIF

.bas.sbr :
!IF $(DEBUG)
	$(BC) /Z $(BCFLAGS_G) $(BCFLAGS_D) $<, nul;
!ELSE
	$(BC) /Z $(BCFLAGS_G) $(BCFLAGS_R) $<, nul;
!ENDIF


run: $(PROJ).exe
	$(PROJ).exe $(RUNFLAGS)

debug: $(PROJ).exe
	CV $(CVFLAGS) $(PROJ).exe $(RUNFLAGS)
