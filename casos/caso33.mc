int total;
total = 0;
int fin;
fin = 0;

char in;
while (! fin){
  in = getchar();
  if(in == '0'){ total = total*10 + 0; }
  else { if(in == '1'){ total = total*10 + 1; }
  else { if(in == '2'){ total = total*10 + 2; }
  else { if(in == '3'){ total = total*10 + 3; }
  else { if(in == '4'){ total = total*10 + 4; }
  else { if(in == '5'){ total = total*10 + 5; }
  else { if(in == '6'){ total = total*10 + 6; }
  else { if(in == '7'){ total = total*10 + 7; }
  else { if(in == '8'){ total = total*10 + 8; }
  else { if(in == '9'){ total = total*10 + 9; }
  else { fin = 1; }; }; }; }; }; }; }; }; }; };
};
int digit;
while (! (total == 0)) {
  digit = total % 10;
  total = total / 10;
  if(digit == 0){ putchar('0'); }
  else { if(digit == 1){ putchar('1'); }
  else { if(digit == 2){ putchar('2'); }
  else { if(digit == 3){ putchar('3'); }
  else { if(digit == 4){ putchar('4'); }
  else { if(digit == 5){ putchar('5'); }
  else { if(digit == 6){ putchar('6'); }
  else { if(digit == 7){ putchar('7'); }
  else { if(digit == 8){ putchar('8'); }
  else { if(digit == 9){ putchar('9'); }
  else {  }; }; }; }; }; }; }; }; }; };
};
