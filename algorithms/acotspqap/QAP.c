/*

       AAAA    CCCC   OOOO    QQQQ   AAAA  PPPPP
      AA  AA  CC     OO  OO  QQ  QQ AA  AA PP  PP
      AAAAAA  CC     OO  OO  QQ  QQ AA  AA PPPPP
      AA  AA  CC     OO  OO  QQ  QQ AA  AA PP
      AA  AA   CCCC   OOOO    QQQQQ  AA  AA PP
                                   Q
######################################################
##########    ACO algorithms for the QAP    ##########
######################################################

      Version: 1.0
      File:    QAP.c
      Author:  Thomas Stuetzle, Manuel Lopez-Ibanez
      Purpose: QAP specific procedures for input, objective function etc.
      Check:   README and gpl.txt
      Copyright (C) 2002, 2014 Thomas Stuetzle, Manuel Lopez-Ibanez
*/

/***************************************************************************

    Program's name: acotsp

    Ant Colony Optimization algorithms (AS, ACS, EAS, RAS, MMAS, BWAS) for the 
    symmetric TSP 

    Copyright (C) 2004  Thomas Stuetzle

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    email: stuetzle no@spam ulb.ac.be
    mail address: Universite libre de Bruxelles
                  IRIDIA, CP 194/6
                  Av. F. Roosevelt 50
                  B-1050 Brussels
		  Belgium

***************************************************************************/


#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "InOut.h"
#include "QAP.h"
#include "ants.h"
#include "ls.h"
#include "utilities.h"


long int n;          /* number of cities in the instance to be solved */
long int null_diagonal_flag  = FALSE;  /* at least one matrix has zero diagonal: TRUE */
long int d_symmetric_flag    = FALSE;  /* if first (d) matrix is symmetric: TRUE */
long int f_symmetric_flag    = FALSE;  /* if second (f) matrix is symmetric: TRUE */
long int make_symmetric_flag = FALSE;  /* convert asymmetric instance into symmetric 
					  instance: TRUE */


struct problem instance;


long int compute_tour_length(const long int *p )
{
/*    
      FUNCTION:      computes the objective function value of a permutation
      INPUT:         pointer to a permutation
      OUTPUT:        none
      (SIDE)EFFECTS: none
      COMMENTS:      Division by 2 has to be done if we have an asymmetric instance that has 
                     been converted into a symmetric one (indicated by make_symmetric_flag). 
		     This is due to the particular way of doing this conversion.
*/
   long int  i, j;
   unsigned long  obj_f_value; /* unsigned, because with only "long int" we have an overflow
				  on some few instances, for example, tai100b. This is because 
				  of making this instance symmetric (see make_matrix_symmetric) 
			       */
   obj_f_value = 0;
   for ( i = 0 ; i < n ; i++ ) {
     for ( j = 0 ; j < n ; j++ ) {
       obj_f_value += instance.distance[i][j] * instance.flow[p[i]][p[j]];
     }
   }
   if ( make_symmetric_flag ) 
     obj_f_value /= 2;

   trace_print("objective function value = %lu \n\n", obj_f_value);
   return obj_f_value;
}

long int compute_evaluation_function (long int *p)
{
    return compute_tour_length (p);
}



static long int read_problem_size (FILE *input)
/* 
      FUNCTION:      read the dimension of the QAP instance
      INPUT:         a pointer to the input file 
      OUTPUT:        the instance dimension (= number of locations / objects)
*/
{
    char buffer[256];
    long int problem_size;
    
    if (fgets (buffer, 255, input) == buffer) {
        /* Some instances have the best-known value after the instance size,
           but we do not use it.  */
        long int best = -1;
        int result = sscanf (buffer, "%ld %ld", &problem_size, &best);
        if (result == 1 || result == 2) {
            trace_print("QAP instance size %ld\n", problem_size);
            if (result == 2)
                trace_print("QAP instance best-known value: %ld\n", best);
            return problem_size;
        }
    }
    fprintf(stderr, "error reading qap size value in data file\n");
    exit(1);
}


long int ** read_matrix( FILE *input, long int size )
/*    
      FUNCTION:      read a QAP instance matrix from the input file
      INPUT:         Pointer to input file, size of QAP instance
      OUTPUT:        pointer to matrix, has to be freed before program stops
      (SIDE)EFFECTS: allocates a memory of appropriate size for the matrix
*/
{
  long int     i, j;
  long int     **matrix;

  if((matrix = malloc(sizeof(long int) * size * size +
		      sizeof(long int *) * size	 )) == NULL){
    fprintf(stderr,"Out of memory, exit.\n"); 
    exit(1);
  }
  for ( i = 0 ; i < size ; i++ ) {
    matrix[i] = (long int *)(matrix + size) + i*size;
    for ( j = 0  ; j < size ; j++ ) {
        if (fscanf(input, "%ld", &matrix[i][j]) != 1) {
            char buffer[64];
            if (fscanf (input, "%60[^ \t\r\n]", buffer) == EOF) {
                fprintf (stderr, "error reading matrix in data file at (%ld, %ld): "
                         "read error or EOF", i, j);
            } else {
                fprintf (stderr, "error reading matrix in data file at (%ld, %ld): "
                         "could not convert string `%s' to long int", 
                         i, j, buffer);
            }
            exit(1);	
        }
    }
  }
  return matrix;
}

bool check_solution(const long int *t)
{
    const int size = n;

    if (!check_permutation (t, size)) {
        fprintf(stderr,"\n%s:error: solution_vector:", __FUNCTION__);
        vector_long_fprint (stderr, t, size);
        fprintf(stderr,"\n");
        return false;
    }
    return true;
}


static void make_matrix_symmetric( long int **matrix, long int size )
/* 
      FUNCTION:      makes an asymmetric matrix symmetric (calculates M = M + M-transpose)
      INPUT:         pointer to the matrix
      OUTPUT:        none
      (SIDE)EFFECTS: makes the Matrix matrix symmetric 
      NOTES:         was described in the 1994 overview paper on QAP by Pardalos, Rendl, Wolkowicz 
*/
{
  long int  i, j;  /* index variables */
  long int  help;
  
  for ( i = 0 ; i < size ; i++ ) {
    for ( j = 0 ; j < i ; j++ ) {
      help = matrix[i][j] + matrix[j][i];
      matrix[i][j] = help;
      matrix[j][i] = help;
    }
  } 
}

void read_instance (const char* filename, struct problem *instance)
{
   FILE *instance_file;

   if ( (instance_file = fopen(filename, "r")) == NULL) {
       fprintf(stderr, "error opening input file %s\n", filename);
       exit(1);	
   }
   /* read instance data */
   n = read_problem_size( instance_file );
    instance->distance = read_matrix( instance_file, n);
    instance->flow = read_matrix( instance_file, n );
#if TRACE
    /*matrix_long_print( instance->distance, n, n);
      matrix_long_print( instance->flow, n, n); */
#endif

    d_symmetric_flag = check_symmetry ( instance->distance, n );
    null_diagonal_flag = check_null_diagonal ( instance->distance, n );   
    /* check for null-diagonal; make symmetric if possible (at most one asymmetric matrix) */
    f_symmetric_flag = check_symmetry ( instance->flow, n );
    /* if one matrix has already null diagonal we need not check the other */
    if (!null_diagonal_flag )
	null_diagonal_flag = check_null_diagonal ( instance->flow, n );

    trace_print("d_symmetric_flag %ld, f_symmetric_flag %ld, null_diagonal_flag %ld\n",
                   d_symmetric_flag, f_symmetric_flag, null_diagonal_flag);

    make_symmetric_flag = XOR(d_symmetric_flag, f_symmetric_flag);
    if ( make_symmetric_flag && null_diagonal_flag ) {
	if ( !d_symmetric_flag )
	    make_matrix_symmetric ( instance->distance, n );
	else if ( !f_symmetric_flag )
	    make_matrix_symmetric ( instance->flow, n );
	else {
	    fprintf(stderr,"One matrix should have been symmetric\n");
	    exit(1);
	}
    }

    strncpy(instance->name, filename, LINE_BUF_LEN);
    instance->name[LINE_BUF_LEN-1] = '\0';
}

void free_instance (struct problem *instance)
{
    free( instance->distance );
    free( instance->flow );
}

const char * get_instance_name(const struct problem *instance)
{
    return instance->name;
}

long int ** compute_nn_lists(struct problem *instance)
{
    instance->nn_list = NULL; /* No nn_lists */
    return instance->nn_list;
}

void printHeur(void)
/*    
      FUNCTION:       print heuristic information 
      INPUT:          none
      OUTPUT:         none
*/
{
  long int i, j;

  printf("Heuristic information:\n");
  for ( i = 0 ; i < n ; i++) {
    printf("From %ld:  ",i);
    for ( j = 0 ; j < n - 1 ; j++ ) {
      printf(" %.3f ", HEURISTIC(i,j));
    }
    printf(" %.3f\n", HEURISTIC(i,j));
    printf("\n");
  }
  printf("\n");
}

void printDist(void) 
/*    
      FUNCTION:       print distance matrix 
      INPUT:          none
      OUTPUT:         none
*/
{
  long int i,j;

  printf("Distance Matrix:\n");
  for ( i = 0 ; i < n ; i++) {
    printf("From %ld:  ",i);
    for ( j = 0 ; j < n - 1 ; j++ ) {
      printf(" %ld", instance.distance[i][j]);
    }
    printf(" %ld\n", instance.distance[i][n-1]);
    printf("\n");
  }
  printf("\n");
}
