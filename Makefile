##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile that stack
##

TARGET			=	leviator

LVT_COMPILER	=	lvtc
LVT_RUNER		=	lvtrun

$(TARGET):
	"$(MAKE)" -C "$(LVT_COMPILER)"
	"$(MAKE)" -C "$(LVT_RUNER)"

debug:
	"$(MAKE)" -C "$(LVT_COMPILER)" debug
	"$(MAKE)" -C "$(LVT_RUNER)" debug

clean:
	"$(MAKE)" -C "$(LVT_COMPILER)" clean
	"$(MAKE)" -C "$(LVT_RUNER)" clean

fclean: clean
	"$(MAKE)" -C "$(LVT_COMPILER)" fclean
	"$(MAKE)" -C "$(LVT_RUNER)" fclean

re: fclean $(TARGET)

tests:
	"$(MAKE)" -C "$(LVT_COMPILER)" tests
	"$(MAKE)" -C "$(LVT_RUNER)" tests

tests-coverage:
	"$(MAKE)" -C "$(LVT_COMPILER)" tests-coverage
	"$(MAKE)" -C "$(LVT_RUNER)" tests-coverage

tests-coverage-html-path:
	@"$(MAKE)" -C "$(LVT_COMPILER)" tests-coverage-html-path
	@"$(MAKE)" -C "$(LVT_RUNER)" tests-coverage-html-path

.PHONY: $(TARGET) fclean re clean all
