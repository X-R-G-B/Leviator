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
	echo $(shell stack path --local-install-root)
	$(CP) "$(shell stack path --local-install-root)/bin/$(BIN_STACK)" "$(BIN_TARGET)"
	$(CP) "$(BIN_TARGET)" "$(MARVIN_TARGET)"

clean:
	stack purge

fclean: clean
	$(RM) "$(BIN_TARGET)"
	$(RM) "$(MARVIN_TARGET)"

re: fclean $(TARGET)

.PHONY: $(TARGET) fclean re clean all
