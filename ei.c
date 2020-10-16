//Pronouns:
//Nominative: ei
//Accusative: eirn
//Pronominal: eirn
//Predicative: eirns
//Reflexive: eirnself

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*4)&(t>>10))|((t*10)&(t>>9)))|((t*10)&(t>>9)))|((t*13)&(t>>7)))^((t*21)&(t>>3)))&23)-1));
}
