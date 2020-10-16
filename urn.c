//Pronouns:
//Nominative: urn
//Accusative: xehx
//Pronominal: xehx
//Predicative: xehxs
//Reflexive: xehxselves

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*8)&(t>>10))^((t*11)&(t>>6)))/((((t*11)&(t>>6))>>4)+2))^((t*14)&(t>>4)))^((t*27)&(t>>7)))&255)%180-1));
}
