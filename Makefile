WHERE 	:=	$(shell stack path --local-install-root)
NAME 			= 	wolfram

all:
	stack build
	cp $(WHERE)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack purge

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re

