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

ABINLINE int int_binwrite(File *fp, int n) {return abfwrite(&n,sizeof(n),1,fp) - 1;}
ABINLINE int ptr_binwrite(File *fp, void *p) {return abfwrite(&p,sizeof(p),1,fp) - 1;}

ABINLINE int string_binwrite(File *fp, char *s)
{
    int n;
    if(!s)
        return int_binwrite(fp,0);
    n = strlen(s)+1;
    return int_binwrite(fp,n)
        + abfwrite(s,sizeof(*s)*n,1,fp) - 1; // include NULL for fun
}

ABINLINE int int_binread(File *fp, int *n)  {return abfread(n,sizeof(*n),1,fp) - 1;}
ABINLINE int ptr_binread(File *fp, intptr_t *n)  {return abfread(n,sizeof(*n),1,fp) - 1;}

ABINLINE int string_binread(File *fp, char **s)
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
    return abfread(*s,sizeof(**s)*(n),1,fp) - 1; // include NULL for fun
}

ABINLINE int mem_binwrite(File *fp, void *d, int n)
{
    if(0!=int_binwrite(fp,n))
        return -1;
    if(!d)
        return -(n != 0); // if 0 bytes written, success.
    return abfwrite(d,n,1,fp) - 1;
}

ABINLINE int mem_binread(File *fp, void **d, int *n_d)
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
    return abfread(*d,n,1,fp) - 1;
}



#endif //ABSERIALIZE_H
