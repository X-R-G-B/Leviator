##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile that stack
##

TARGET			=	koaky
MARVIN_TARGET	=	glados

CP			=	cp
RM			=	rm -rf

ifeq ($(OS),Windows_NT)
	BIN_STACK	=	$(TARGET)-exe.exe
else
	BIN_STACK	=	$(TARGET)-exe
endif

all: $(TARGET)

$(TARGET):
	stack build --copy-bins --local-bin-path .
	$(CP) "$(BIN_STACK)" "$(MARVIN_TARGET)"

clean:
	stack clean

fclean: clean
	stack purge
	$(RM) "$(BIN_STACK)"
	$(RM) "$(MARVIN_TARGET)"

re: fclean $(TARGET)

.PHONY: $(TARGET) fclean re clean all
