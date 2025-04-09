%{
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>

	void yyerror(char* s);
	int yylex();
	void ins();
	void insV();
	int flag=0;

	extern char curid[20];
	extern char curtype[20];
	extern char curval[20];

	extern int currnest;
	void deletedata (int );
	int checkscope(char*);
	void insertST(char*, char*);
	void insertSTnest(char*, int);
	int duplicate(char *s);
	char currfunctype[100];
	char currfunc[100];
	void insertSTF(char*);
	char gettype(char*,int);
	void push(char *s);
	void codegen();
	void codeassign();
	char* itoa(int num, char* str, int base);
	void reverse(char str[], int length);
	void swap(char*,char*);
	void label1();
	void label2();
	void label3();
	void genunary();
	void codegencon();
	void funcgen();
	void funcgenend();

	FILE *fp;

	int top = 0,count=0,ltop=0,lno=0;
	char temp[3] = "t";
%}

%token INT CHAR FLOAT DOUBLE LONG SHORT SIGNED UNSIGNED
%token RETURN MAIN
%token VOID
%token WHILE FOR DO
%token BREAK


%token identifier
%token integer_constant string_constant float_constant character_constant




%right MOD_ASSIGN_OP
%right MUL_ASSIGN_OP DIV_ASSIGN_OP
%right ADD_ASSIGN_OP SUB_ASSIGN_OP
%right ASSIGN_OP

%left OR_OP
%left AND_OP
%left EQ_OP NE_OP
%left LT_ASSIGN_OP LT_OP GT_ASSIGN_OP GT_OP
%left ADD_OP SUB_OP
%left MUL_OP DIV_OP MOD_OP

%right SIZEOF
%right exclamation_OP
%left INC_OP DEC_OP


%start program

%%
program
			: function_declaration;

variable_declaration
			: type_specifier variable_declaration_list ';';

variable_declaration_list
			: variable_declaration_list ',' variable_declaration_identifier | variable_declaration_identifier;

variable_declaration_identifier
			: identifier {if(duplicate(curid)){printf("Duplicate\n");}insertSTnest(curid,currnest); ins();  } vdi;


vdi : ASSIGN_OP simple_expression | ;

type_specifier
			: INT | CHAR | FLOAT  | DOUBLE
			| LONG long_grammar
			| SHORT short_grammar
			| UNSIGNED unsigned_grammar
			| SIGNED signed_grammar
			| VOID
			;

unsigned_grammar
			: INT | LONG long_grammar | SHORT short_grammar | ;

signed_grammar
			: INT | LONG long_grammar | SHORT short_grammar | ;

long_grammar
			: INT  | ;

short_grammar
			: INT | ;

function_declaration
			: function_declaration_type;

function_declaration_type
			: type_specifier MAIN '('  { strcpy(currfunctype, curtype); strcpy(currfunc, curid); insertSTF(curid); ins(); } ')' {funcgen();} statement {funcgenend();};

statement
			: expression_statment | compound_statement
			| iterative_statements
			| return_statement | break_statement
			| variable_declaration;

compound_statement
			: {currnest++;} '{'  statment_list  '}' {deletedata(currnest);currnest--;};

statment_list
			: statement statment_list
			| statement error ';' {yyerrok;}
			| ;

expression_statment
			: expression ';'
			| ';'
			| error ';' {yyerrok;};

iterative_statements
			: WHILE '(' {label2();} simple_expression ')' {label1();if($4!=1){printf("Condition checking is not of type int\n");}} statement {label3();}
			| FOR '(' expression ';' {label2();} simple_expression ';' {label1();if($6!=1){printf("Condition checking is not of type int\n");}} expression ')'statement {label3();}
			| {label2();}DO statement WHILE '(' simple_expression ')'{label1();label3();if($6!=1){printf("Condition checking is not of type int\n");exit(0);}} ';';

return_statement
			: RETURN ';' {if(strcmp(currfunctype,"void")) {printf("Returning void of a non-void function\n");}}
			| RETURN expression ';' { 	if(!strcmp(currfunctype, "void"))
										{
											yyerror("Function is void");
										}

										if((currfunctype[0]=='i' || currfunctype[0]=='c') && $2!=1)
										{
											printf("Expression doesn't match return type of function\n");
										}
									};

break_statement
			: BREAK ';';

expression
			: mutable ASSIGN_OP {push("=");} expression   {
																	  if($1==1 && $4==1)
																	  {
			                                                          $$=1;
			                                                          }
			                                                          else
			                                                          {$$=-1; printf("Type mismatch\n"); }
			                                                          codeassign();
			                                                       }
			| mutable ADD_ASSIGN_OP {push("+=");}expression {
																	  if($1==1 && $4==1)
			                                                          $$=1;
			                                                          else
			                                                          {$$=-1; printf("Type mismatch\n"); }
			                                                          codeassign();
			                                                       }
			| mutable SUB_ASSIGN_OP {push("-=");} expression  {
																	  if($1==1 && $4==1)
			                                                          $$=1;
			                                                          else
			                                                          {$$=-1; printf("Type mismatch\n"); }
			                                                          codeassign();
			                                                       }
			| mutable MUL_ASSIGN_OP {push("*=");} expression {
																	  if($1==1 && $4==1)
			                                                          $$=1;
			                                                          else
			                                                          {$$=-1; printf("Type mismatch\n"); }
			                                                          codeassign();
			                                                       }
			| mutable DIV_ASSIGN_OP {push("/=");}expression 		{
																	  if($1==1 && $4==1)
			                                                          $$=1;
			                                                          else
			                                                          {$$=-1; printf("Type mismatch\n"); }
			                                                       }
			| mutable MOD_ASSIGN_OP {push("%=");}expression 		{
																	  if($1==1 && $3==1)
			                                                          $$=1;
			                                                          else
			                                                          {$$=-1; printf("Type mismatch\n"); }
			                                                          codeassign();
																	}
			| mutable INC_OP 							{ push("++");if($1 == 1) $$=1; else $$=-1; genunary();}
			| mutable DEC_OP  							{push("--");if($1 == 1) $$=1; else $$=-1;  genunary();}
			| INC_OP mutable							{ push("++");if($1 == 1) $$=1; else $$=-1; genunary();}
			| DEC_OP mutable  							{push("--");if($1 == 1) $$=1; else $$=-1;  genunary();} 
			| simple_expression {if($1 == 1) $$=1; else $$=-1;} ;


simple_expression
			: simple_expression OR_OP and_expression {push("||");} {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			| and_expression {if($1 == 1) $$=1; else $$=-1;};

and_expression
			: and_expression AND_OP {push("&&");} unary_relation_expression  {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			  |unary_relation_expression {if($1 == 1) $$=1; else $$=-1;} ;


unary_relation_expression
			: exclamation_OP {push("!");} unary_relation_expression {if($2==1) $$=1; else $$=-1; codegen();}
			| regular_expression {if($1 == 1) $$=1; else $$=-1;} ;

regular_expression
			: regular_expression relational_OPs sum_expression {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			  | sum_expression {if($1 == 1) $$=1; else $$=-1;} ;

relational_OPs
			: GT_ASSIGN_OP {push(">=");} | LT_ASSIGN_OP {push("<=");} | GT_OP {push(">");}| LT_OP {push("<");}| EQ_OP {push("==");}| NE_OP {push("!=");} ;

sum_expression
			: sum_expression sum_OPs term  {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			| term {if($1 == 1) $$=1; else $$=-1;};

sum_OPs
			: ADD_OP {push("+");}
			| SUB_OP {push("-");} ;

term
			: term MULOP factor {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			| factor {if($1 == 1) $$=1; else $$=-1;} ;

MULOP
			: MUL_OP {push("*");}| DIV_OP {push("/");} | MOD_OP {push("%");} ;

factor
			: immutable {if($1 == 1) $$=1; else $$=-1;}
			| mutable {if($1 == 1) $$=1; else $$=-1;} ;

mutable
			: identifier {
						  push(curid);
			              if(!checkscope(curid))
			              {printf("%s\n",curid);printf("Undeclared\n");}
			              if(gettype(curid,0)=='i' || gettype(curid,1)== 'c')
			              $$ = 1;
			              else
			              $$ = -1;
			              };

immutable
			: '(' expression ')' {if($2==1) $$=1; else $$=-1;}
			| constant {if($1==1) $$=1; else $$=-1;};


constant
			: integer_constant 	{  insV(); codegencon(); $$=1; }
			| string_constant	{  insV(); codegencon();$$=-1;}
			| float_constant	{  insV(); codegencon();}
			| character_constant{  insV(); codegencon();$$=1; };

%%

extern FILE *yyin;
extern int yylineno;
extern char *yytext;
void insertSTtype(char *,char *);
void insertSTvalue(char *, char *);
void incertCT(char *, char *);
void printST();
void printCT();

struct stack
{
	char value[100];
	int labelvalue;
}s[100],label[100];


void push(char *x)
{
	strcpy(s[++top].value,x);
}

void swap(char *x, char *y)
{
	char temp = *x;
	*x = *y;
	*y = temp;
}

void reverse(char str[], int length)
{
    int start = 0;
    int end = length -1;
    while (start < end)
    {
        swap((str+start), (str+end));
        start++;
        end--;
    }
}

char* itoa(int num, char* str, int base)
{
    int i = 0;
    int isNegative = 0;


    if (num == 0)
    {
        str[i++] = '0';
        str[i] = '\0';
        return str;
    }

    if (num < 0 && base == 10)
    {
        isNegative = 1;
        num = -num;
    }


    while (num != 0)
    {
        int rem = num % base;
        str[i++] = (rem > 9)? (rem-10) + 'a' : rem + '0';
        num = num/base;
    }

    if (isNegative)
        str[i++] = '-';

    str[i] = '\0';


    reverse(str, i);

    return str;
}

void codegen()
{
  if(flag==0)
	{
	strcpy(temp,"t");
	char buffer[100];
	itoa(count,buffer,10);
	strcat(temp,buffer);
	printf("%s %s %s %s\n",s[top-1].value,s[top-2].value,s[top].value,temp);
  fprintf(fp,"%s=%s%s%s\n",temp,s[top-2].value,s[top-1].value,s[top].value);
	top = top - 2;
	strcpy(s[top].value,temp);
	count++;
	insertST(temp,"Temporary");
	}
}

void codegencon()
{
if(flag==0)
{
	strcpy(temp,"t");
	char buffer[100];
	itoa(count,buffer,10);
	strcat(temp,buffer);
	printf("= %s   %s\n",curval,temp);
	fprintf(fp,"%s=%s\n",temp,curval);
	printf("= %s   %s\n",temp,curid);
	fprintf(fp,"%s=%s\n",curid,temp);
	insertST(temp,"Temporary");
	push(temp);
	count++;
	}

}

int isunary(char *s)
{
	if(strcmp(s, "--")==0 || strcmp(s, "++")==0)
	{
		return 1;
	}
	return 0;
}

void genunary()
{
  if(flag==0)
	{
	char temp1[100], temp2[100], temp3[100];
	strcpy(temp1, s[top].value);
	strcpy(temp2, s[top-1].value);

	if(isunary(temp1))
	{
		strcpy(temp3, temp1);
		strcpy(temp1, temp2);
		strcpy(temp2, temp3);
	}
	strcpy(temp, "t");
	char buffer[100];
	itoa(count, buffer, 10);
	strcat(temp, buffer);
	count++;

	if(strcmp(temp2,"--")==0)
	{
		printf("- %s 1 %s\n", temp1, temp);
		fprintf(fp,"%s=%s-1\n", temp, temp1);
		printf("= %s   %s\n", temp, temp1);
		fprintf(fp,"%s=%s\n", temp1, temp);
	}

	if(strcmp(temp2,"++")==0)
	{
		printf("+ %s 1 %s\n", temp1, temp);
		fprintf(fp,"%s=%s+1\n", temp, temp1);
		printf("= %s   %s\n", temp, temp1);
		fprintf(fp,"%s=%s\n", temp1, temp);
	}
	insertST(temp,"Temporary");
	insertST(temp1,"Temporary");
	top = top -2;
	}
}

void codeassign()
{
  if(flag==0)
	{
	printf("= %s   %s\n",s[top].value,s[top-2].value);
	fprintf(fp,"%s=%s\n",s[top-2].value,s[top].value);
	top = top - 2;
	}
}

void label1()
{
  if(flag==0)
	{
	strcpy(temp,"L");
	char buffer[100];
	itoa(lno,buffer,10);
	strcat(temp,buffer);
	printf("ifnot %s goto %s\n",s[top].value,temp);
	fprintf(fp,"ifnot %s goto %s\n",s[top].value,temp);
	label[++ltop].labelvalue = lno++;
	
	}
}




void label2()
{
	strcpy(temp,"L");
	char buffer[100];
	itoa(lno,buffer,10);
	strcat(temp,buffer);
	printf("%s:\n",temp);
	fprintf(fp,"%s: ",temp);
	label[++ltop].labelvalue = lno++;
}


void label3()
{
  if(flag==0)
	{
	strcpy(temp,"L");
	char buffer[100];
	itoa(label[ltop-1].labelvalue,buffer,10);
	strcat(temp,buffer);
	printf("goto %s:\n",temp);
	fprintf(fp,"goto %s: ",temp);
	strcpy(temp,"L");
	itoa(label[ltop].labelvalue,buffer,10);
	strcat(temp,buffer);
	printf("%s:\n",temp);
	fprintf(fp,"%s: ",temp);
	ltop = ltop - 2;
	}

}

void funcgen()
{
	if(flag==0)
	printf("func begin %s\n",currfunc);
}

void funcgenend()
{
  if(flag==0)
	printf("func end\n\n");
}



int main(int argc , char **argv)
{
	yyin = fopen(argv[1], "r");
	fp = fopen("icg.txt", "w");
	yyparse();
	fclose(fp);
	fclose(yyin);
	if(flag == 0)
	{
		printf("Status: Parsing Complete - Valid\n");
		printf("%30s SYMBOL TABLE\n", " ");
		printf("%30s %s\n", " ", "------------");
		printST();

		printCT();
	}
}

void yyerror(char *s)
{
	printf("%s at lineno: %d at character: %s\n", s, yylineno, yytext);
	flag=1;
	printf("Status: Parsing Failed - Invalid\n");

}

void ins()
{
	insertSTtype(curid,curtype);
}

void insV()
{
	insertSTvalue(curid,curval);
}

int yywrap()
{
	return 1;
}
