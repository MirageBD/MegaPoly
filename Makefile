# -----------------------------------------------------------------------------

megabuild		= 1
useetherload	= 1
finalbuild		= 1
attachdebugger	= 0

# -----------------------------------------------------------------------------

MAKE			= make
CP				= cp
MV				= mv
RM				= rm -f
CAT				= cat

SRC_DIR			= ./src
UI_SRC_DIR		= ./src/ui
UIELT_SRC_DIR	= ./src/ui/uielements
DRVRS_SRC_DIR	= ./src/drivers
EXE_DIR			= ./exe
BIN_DIR			= ./bin

# mega65 fork of ca65: https://github.com/dillof/cc65
AS				= ca65mega
ASFLAGS			= -g -D finalbuild=$(finalbuild) -D megabuild=$(megabuild) -D useetherload=$(useetherload) --cpu 45GS02 -U --feature force_range -I ./exe
LD				= ld65
C1541			= c1541
CC1541			= cc1541
SED				= sed
PU				= pucrunch
BBMEGA			= b2mega
LC				= crush 6
GCC				= gcc
MC				= MegaConvert
MEGAADDRESS		= megatool -a
MEGACRUNCH		= megatool -c
MEGAIFFL		= megatool -i
MEGAMOD			= MegaMod
EL				= etherload -i 192.168.1.255
XMEGA65			= H:\xemu\xmega65.exe
MEGAFTP			= mega65_ftp -i 192.168.1.255

CONVERTBREAK	= 's/al [0-9A-F]* \.br_\([a-z]*\)/\0\nbreak \.br_\1/'
CONVERTWATCH	= 's/al [0-9A-F]* \.wh_\([a-z]*\)/\0\nwatch store \.wh_\1/'

CONVERTVICEMAP	= 's/al //'

.SUFFIXES: .o .s .out .bin .pu .b2 .a

default: all

OBJS = $(EXE_DIR)/boot.o $(EXE_DIR)/main.o

BINFILES = $(BIN_DIR)/bitmap_chars0.bin
BINFILES += $(BIN_DIR)/bitmap_pal0.bin
BINFILES += $(BIN_DIR)/song.mod

BINFILESADDR  = $(BIN_DIR)/bitmap_chars0.bin.addr
BINFILESADDR += $(BIN_DIR)/bitmap_pal0.bin.addr
BINFILESADDR += $(BIN_DIR)/song.mod.addr

BINFILESMC  = $(BIN_DIR)/bitmap_chars0.bin.addr.mc
BINFILESMC += $(BIN_DIR)/bitmap_pal0.bin.addr.mc
BINFILESMC += $(BIN_DIR)/song.mod.addr.mc

# % is a wildcard
# $< is the first dependency
# $@ is the target
# $^ is all dependencies

# -----------------------------------------------------------------------------

$(BIN_DIR)/bitmap_chars0.bin: $(BIN_DIR)/bitmap.bin
	$(MC) $< cm1:1 d1:3 cl1:10000 rc1:0

$(EXE_DIR)/boot.o:	$(SRC_DIR)/boot.s \
					$(SRC_DIR)/main.s \
					$(SRC_DIR)/irqload.s \
					$(SRC_DIR)/decruncher.s \
					$(SRC_DIR)/macros.s \
					$(SRC_DIR)/mathmacros.s \
					$(SRC_DIR)/model.s \
					$(SRC_DIR)/modplay.s \
					Makefile Linkfile
	$(AS) $(ASFLAGS) -o $@ $<

$(BIN_DIR)/alldata.bin: $(BINFILES)
	$(MEGAADDRESS) $(BIN_DIR)/bitmap_chars0.bin      00010000
	$(MEGAADDRESS) $(BIN_DIR)/bitmap_pal0.bin        0000c000
	$(MEGAADDRESS) $(BIN_DIR)/song.mod               00040000
	$(MEGACRUNCH) $(BIN_DIR)/bitmap_chars0.bin.addr
	$(MEGACRUNCH) $(BIN_DIR)/bitmap_pal0.bin.addr
	$(MEGACRUNCH) $(BIN_DIR)/song.mod.addr
	$(MEGAIFFL) $(BINFILESMC) $(BIN_DIR)/alldata.bin

$(EXE_DIR)/boot.prg.addr: $(EXE_DIR)/boot.o Linkfile
	$(LD) -Ln $(EXE_DIR)/boot.maptemp --dbgfile $(EXE_DIR)/boot.dbg -C Linkfile -o $(EXE_DIR)/boot.prg $(EXE_DIR)/boot.o
	$(MEGAADDRESS) $(EXE_DIR)/boot.prg 2001
	$(SED) $(CONVERTVICEMAP) < $(EXE_DIR)/boot.maptemp > boot.map
	$(SED) $(CONVERTVICEMAP) < $(EXE_DIR)/boot.maptemp > boot.list

$(EXE_DIR)/megapoly.d81: $(EXE_DIR)/boot.prg.addr $(BIN_DIR)/alldata.bin
	$(RM) $@
	$(CC1541) -n "megapoly" -i " 2023" -d 19 -v\
	 \
	 -f "megapoly" -w $(EXE_DIR)/boot.prg.addr \
	 -f "megaply.ifflcrch" -w $(BIN_DIR)/alldata.bin \
	$@

# -----------------------------------------------------------------------------

run: $(EXE_DIR)/megapoly.d81

ifeq ($(megabuild), 1)

ifeq ($(useetherload), 1)

	$(MEGAFTP) -c "put D:\Mega\MegaPoly\exe\megapoly.d81 megapoly.d81" -c "quit"
	$(EL) -m MEGAPOLY.D81 -r $(EXE_DIR)/boot.prg.addr

else

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

endif

ifeq ($(attachdebugger), 1)
	m65dbg --device /dev/ttyS2
endif

else

	cmd.exe /c $(XMEGA65) -autoload -8 $(EXE_DIR)/megapoly.d81

endif

clean:
	$(RM) $(EXE_DIR)/*.*
	$(RM) $(EXE_DIR)/*
