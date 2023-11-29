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
	BIN_TARGET	=	$(TARGET).exe
else
	BIN_STACK	=	$(TARGET)-exe
	BIN_TARGET	=	$(TARGET)
endif

all: $(TARGET)

$(TARGET):
ifeq ($(OS),Windows_NT)
	./scripts/Build.ps1 $(BIN_STACK) $(BIN_TARGET)
else
	./scripts/Build.sh $(BIN_STACK) $(BIN_TARGET)
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
