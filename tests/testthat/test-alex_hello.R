if (require(alexandershemetev)){
a <- alex_hello()
expect_match(a, "Hello, Dear User!")
}