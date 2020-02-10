(a <- letters[1:10])
names(a) <- LETTERS[1:10]
a
get_element(a, 4)
get_element(a, 'D')
rm(a)

skip_null(NULL)
skip_null(NULL, 'fuck')