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

ABINLINE int binwrite_int(FILE *fp, int n)
{
    return fwrite(&n,sizeof(n),1,fp) - 1;
}

ABINLINE int binwrite_string(FILE *fp, char *s)
{
    int n;
    if(!s)
        return binwrite_int(fp,0);
    n = strlen(s)+1;
    return binwrite_int(fp,n)
        + fwrite(s,sizeof(*s)*n,1,fp) - 1; // include NULL for fun
}

ABINLINE int binread_int(FILE *fp, int *n)
{
    return fread(n,sizeof(*n),1,fp) - 1;
}

ABINLINE int binread_string(FILE *fp, char **s)
{
    int n;
    if(binread_int(fp,&n) < 0 || n < 0)
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


#endif //ABSERIALIZE_H
