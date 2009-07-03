/***************************************************************************
 *     Copyright (c) 2009-2009, Aaron Brady
 *     All Rights Reserved
 *
 * Module Description:
 *
 *
 ***************************************************************************/
#ifndef ABSERIALIZE_H
#define ABSERIALIZE_H

ABINLINE int int_binwrite(FILE *fp, int n) {return fwrite(&n,sizeof(n),1,fp) - 1;}
ABINLINE int ptr_binwrite(FILE *fp, void *p) {return fwrite(&p,sizeof(p),1,fp) - 1;}

ABINLINE int string_binwrite(FILE *fp, char *s)
{
    int n;
    if(!s)
        return int_binwrite(fp,0);
    n = strlen(s)+1;
    return int_binwrite(fp,n)
        + fwrite(s,sizeof(*s)*n,1,fp) - 1; // include NULL for fun
}

ABINLINE int int_binread(FILE *fp, int *n)  {return fread(n,sizeof(*n),1,fp) - 1;}
ABINLINE int ptr_binread(FILE *fp, intptr_t *n)  {return fread(n,sizeof(*n),1,fp) - 1;}

ABINLINE int string_binread(FILE *fp, char **s)
{
    int n;
    if(int_binread(fp,&n) < 0 || n < 0)
        return -1;
    else if(n == 0)
    {
        if(*s)
            free(*s);
        *s = 0;
        return 0;
    }
    (*s) = realloc(*s,n);
    return fread(*s,sizeof(**s)*(n),1,fp) - 1; // include NULL for fun
}

ABINLINE int mem_binwrite(FILE *fp, void *d, int n)
{
    if(0!=int_binwrite(fp,n))
        return -1;
    if(!d)
        return -(n != 0); // if 0 bytes written, success.
    return fwrite(d,n,1,fp) - 1;
}

ABINLINE int mem_binread(FILE *fp, void **d, int *n_d)
{
    int n;
    if(int_binread(fp,&n) < 0 || n < 0)
        return -1;
    if(n_d)
        *n_d = n;
    if(n == 0)
    {
        if(*d)
            free(*d);
        *d = 0;
        return 0;
    }
    *d = realloc(*d,n);
    return fread(*d,n,1,fp) - 1;
}



#endif //ABSERIALIZE_H
