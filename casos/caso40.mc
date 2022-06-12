int total;
total = 0;
int fin;

char in;
int num;

int rev;
int resto;

int digit;

char op;

fin = 0;
while (! fin){
  num = 0;
  fin = 0;
  while (! fin){
    in = getchar();
    if(in == '0'){ num = num*10 + 0; }
    else { if(in == '1'){ num = num*10 + 1; }
    else { if(in == '2'){ num = num*10 + 2; }
    else { if(in == '3'){ num = num*10 + 3; }
    else { if(in == '4'){ num = num*10 + 4; }
    else { if(in == '5'){ num = num*10 + 5; }
    else { if(in == '6'){ num = num*10 + 6; }
    else { if(in == '7'){ num = num*10 + 7; }
    else { if(in == '8'){ num = num*10 + 8; }
    else { if(in == '9'){ num = num*10 + 9; }
    else { fin = 1; }; }; }; }; }; }; }; }; }; };
  };
  total = num;

  op = getchar();

  num = 0;
  fin = 0;
  while (! fin){
    in = getchar();
    if(in == '0'){ num = num*10 + 0; }
    else { if(in == '1'){ num = num*10 + 1; }
    else { if(in == '2'){ num = num*10 + 2; }
    else { if(in == '3'){ num = num*10 + 3; }
    else { if(in == '4'){ num = num*10 + 4; }
    else { if(in == '5'){ num = num*10 + 5; }
    else { if(in == '6'){ num = num*10 + 6; }
    else { if(in == '7'){ num = num*10 + 7; }
    else { if(in == '8'){ num = num*10 + 8; }
    else { if(in == '9'){ num = num*10 + 9; }
    else { fin = 1; }; }; }; }; }; }; }; }; }; };
  };

  if(op == 'a'){ total = total + num;}
  else { if(op == 's'){ total = total - num;}
  else { if(op == 'm'){ total = total * num;}
  else { if(op == 'd'){ total = total / num;}
  else { total = total % num; }; }; };};

  rev = 0;
  while (total){
    resto = total % 10;
    rev = rev * 10 + resto;
    total = total / 10;
  };

  while (! (rev == 0)) {
    digit = rev % 10;
    rev   = rev / 10;
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
  
  op = getchar();
  if (op == 'q') { fin = 1; }
  else {fin = 0; };
};
