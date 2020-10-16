//Pronouns:
//Nominative: um
//Accusative: spriem
//Pronominal: sprom
//Predicative: spriems
//Reflexive: spriemself

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*5)&(t>>3))^((t*16)&(t>>10)))^((t*14)&(t>>5)))^((t*19)&(t>>8)))^((t*27)&(t>>4)))&255)-1));
}
