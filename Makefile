# -----------------------------------------------------------------------------

megabuild		= 1
finalbuild		= 1
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f

SRC_DIR			= ./src
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
C1541			= c1541
CC1541			= cc1541
SED				= sed
PU				= pucrunch
BBMEGA			= b2mega
LC				= crush 6
GCC				= gcc
MC				= MegaConvert
ADDADDR			= addaddr
XMEGA65			= F:\xemu\xmega65.exe

CONVERTBREAK	= 's/al [0-9A-F]* \.br_\([a-z]*\)/\0\nbreak \.br_\1/'
CONVERTWATCH	= 's/al [0-9A-F]* \.wh_\([a-z]*\)/\0\nwatch store \.wh_\1/'

CONVERTVICEMAP	= 's/al //'

.SUFFIXES: .o .s .out .bin .pu .b2 .a

default: all

OBJS = $(EXE_DIR)/boot.o $(EXE_DIR)/main.o

# % is a wildcard
# $< is the first dependency
# $@ is the target
# $^ is all dependencies

# -----------------------------------------------------------------------------

$(BIN_DIR)/bmp_charset.bin: $(BIN_DIR)/bitmap.bin
	$(MC)
	$(MC) $< cm1:1 d1:3 cl1:20000 rc1:0

$(EXE_DIR)/boot.o:	$(SRC_DIR)/boot.s \
					$(SRC_DIR)/main.s \
					$(SRC_DIR)/irqload.s \
					$(SRC_DIR)/macros.s \
					$(SRC_DIR)/mathmacros.s \
					$(SRC_DIR)/model.s \
					Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

$(EXE_DIR)/boot.prg: $(EXE_DIR)/boot.o Linkfile
	$(LD) -Ln $(EXE_DIR)/boot.maptemp --dbgfile $(EXE_DIR)/boot.dbg -C Linkfile -o $@ $(EXE_DIR)/boot.o
	$(ADDADDR) $(EXE_DIR)/boot.prg $(EXE_DIR)/bootaddr.prg 8193
	$(SED) $(CONVERTVICEMAP) < $(EXE_DIR)/boot.maptemp > boot.map
	$(SED) $(CONVERTVICEMAP) < $(EXE_DIR)/boot.maptemp > boot.list

$(EXE_DIR)/megapoly.d81: $(EXE_DIR)/boot.prg $(BIN_DIR)/bmp_charset.bin
	$(RM) $@
	$(CC1541) -n "megapoly" -i " 2023" -d 19 -v\
	 \
	 -f "boot" -w $(EXE_DIR)/bootaddr.prg \
	 -f "00" -w $(BIN_DIR)/bitmap_chars0.bin \
	 -f "01" -w $(BIN_DIR)/bitmap_pal0.bin \
	$@

# -----------------------------------------------------------------------------

run: $(EXE_DIR)/megapoly.d81

ifeq ($(megabuild), 1)

	m65 -l COM3 -F
	mega65_ftp.exe -l COM3 -s 2000000 -c "cd /" \
	-c "put D:\Mega\MegaPoly\exe\megapoly.d81 megapoly.d81"

	m65 -l COM3 -F
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'mount "megapoly.d81"'
	m65 -l COM3 -T 'load "$$"'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'load "boot"'
	m65 -l COM3 -T 'list'
	m65 -l COM3 -T 'run'

ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif

else

	cmd.exe /c $(XMEGA65) -autoload -8 $(EXE_DIR)/megapoly.d81

endif



clean:
	$(RM) $(EXE_DIR)/*.*
	$(RM) $(EXE_DIR)/*

