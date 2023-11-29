##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile that stack
##

TARGET			=	koaky
MARVIN_TARGET	=	glados

ifeq ($(OS),Windows_NT)
	CP			=	Copy-Item
	RM			=	Remove-Item -Force -Recurse
	BIN_STACK	=	$(TARGET)-exe.exe
	BIN_TARGET	=	$(TARGET).exe
else
	CP			=	cp
	RM			=	rm -rf
	BIN_STACK	=	$(TARGET)-exe
	BIN_TARGET	=	$(TARGET)
endif

all: $(TARGET)

$(TARGET):
	stack build
ifeq ($(OS),Windows_NT)
	$(CP) "(stack path --local-install-root)/bin/$(BIN_STACK)" "$(BIN_TARGET)"
else
	$(CP) "$(shell stack path --local-install-root)/bin/$(BIN_STACK)" "$(BIN_TARGET)"
endif
	$(CP) "$(BIN_TARGET)" "$(MARVIN_TARGET)"

clean:
	stack clean

fclean: clean
	stack purge
	$(RM) "$(BIN_TARGET)"
	$(RM) "$(MARVIN_TARGET)"

re: fclean $(TARGET)

.PHONY: $(TARGET) fclean re clean all
