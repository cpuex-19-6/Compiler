#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
  char buf[20];
  char rd[5], rs1[5], rs2[5], imm[10];
  FILE *in, *out;

  if (argc != 3) {
    printf("Usage: ./assembler [assembly path] [binary path]");
    exit(1);
  }

  in = fopen(argv[1], "r");
  if (in == NULL) {
    perror("fopen() error");
    exit(1);
  }
  out = fopen(argv[2], "w");
  if (out == NULL) {
    perror("fopen() error");
    exit(1);
  }

  while(fscanf(in, "%s", buf) != EOF) {
    // 1st architecture

    if (strcmp(buf, "lui") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 

    else if (strcmp(buf, "addi") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "ori") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf     
    }
    else if (strcmp(buf, "andi") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf     
    }
    else if (strcmp(buf, "add") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      // TODO: fprintf
    }
    else if (strcmp(buf, "sub") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "or") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "and") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      // TODO: fprintf
    } 

    else if (strcmp(buf, "lbu") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    }    
    else if (strcmp(buf, "lhu") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    }  
    else if (strcmp(buf, "lb") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "lh") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "lw") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "sb") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "sh") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "sw") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 

    else if (strcmp(buf, "auipc") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "jalr") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "jal") == 0) {
      fscanf(in, "%s", rd);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 

    else if (strcmp(buf, "bltu") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "bgeu") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "beq") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "bne") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "blt") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 
    else if (strcmp(buf, "bge") == 0) {
      fscanf(in, "%s", rs1);
      fscanf(in, "%s", rs2);
      fscanf(in, "%s", imm);
      // TODO: fprintf
    } 

    else {
      // printf("skip: %s\n", buf);
    }
  }

  fclose(in);
  fclose(out);
  return 0;
}