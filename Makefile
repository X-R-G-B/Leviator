##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile that stack
##

TARGET			=	koaky
MARVIN_TARGET	=	glados

all: $(TARGET)

$(TARGET):
	stack build
	cp "$(shell stack path --local-install-root)/bin/$(TARGET)-exe" "$(TARGET)"
	cp "$(TARGET)" "$(MARVIN_TARGET)"

clean:
	stack purge

fclean: clean
	$(RM) "$(TARGET)"
	$(RM) "$(MARVIN_TARGET)"

re: fclean "$(TARGET)"

.PHONY: $(TARGET) fclean re clean all
