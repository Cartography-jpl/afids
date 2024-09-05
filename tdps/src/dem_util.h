#ifndef ET_UTIL_H
#define ET_UTIL_H
#ifndef max
#define max(a,b)    (((a) > (b)) ? (a) : (b))
#endif
#ifndef min
#define min(a,b)    (((a) < (b)) ? (a) : (b))
#endif

#ifndef IN_T
#ifdef I64
#define IN_T long long
#else
#define IN_T int
#endif
#endif


typedef char *Ptr;
typedef IN_T (*PFI) ();               /* pointer to func returning IN_T */
typedef struct stable {
    long s_entries;                  /* number of entries in hash array */
    long s_hval;                     /* after lookup, the hash value */
    struct  sblock *s_block;         /*               and the block addr */
    long s_user[4];                  /* just 4 cells for the user to use */
    long s_spare[2];
	long s_hash_alg;				 /* which of a set of possible hash algorithms to use */
    long s_nins;                     /* number of calls to install */
    long s_colls;                    /* number of collisions */
    long s_ncmps;                    /* number of compares on lookups */
    long s_look;                     /* number of lookups */
    long s_nfnd;                     /* number of lookups not found */
    struct sblock *s_hash[1];        /* variable length */
    } SYMTABLE, *PSYMTABLE, *SYMTABLE_PTR;

typedef struct sblock {
    char *b_value;                   /* data pointer must be non NULL */
    long b_refs;                     /* Look up references    */
      union {                        /* long b_spare[2]; */
            char b_symbol[1];        /* var len string */
            IN_T  i_symbol;
          } sym_type;
    } SYMBLOCK, *PSYMBLOCK, *SYMBLOCK_PTR;


#define do_all(__type,__index,__lh) \
      __index=__lh ;for (;;) {__index=(__type *)list_next(__index); if (__index == __lh) break;
#define od_all }


#define     for_list(_type,_index,_list)  for(_index = (_type *)list_next(_list) ; _index!=(_type *)_list ; _index = (_type *)list_next(_index))
#define for_list_rev(_type,_index,_list)  for(_index = (_type *)list_prev(_list) ; _index!=(_type *)_list ; _index = (_type *)list_prev(_index))


/*
#define     for_list(_type,_index,_list)  for(_index = (_type *)_list->next() ; _index!=(_type *)_list ; _index=(_type *)_index->next())
#define for_list_rev(_type,_index,_list)  for(_index = (_type *)_list->prev() ; _index!=(_type *)_list ; _index=(_type *)_index->prev())

#define     do_list(_type,_index,_list) _index = (_type *)_list; for(;;){ _index=(_type *)_index->Next();if(_index==(_type *)_list)break;
#define do_list_rev(_type,_index,_list) _index = (_type *)_list; for(;;){ _index=(_type *)_index->Prev();if(_index==(_type *)_list)break;
*/

typedef struct  atom    { /* this structure precedes what the user sees us return */
#ifdef LL_TRACKALL
		struct atom    *all_right; // used to link all memory allocated
		struct atom    *all_left;
#endif
#ifdef LL_ID
		char name[16];
#endif
        struct atom    *right;
        struct atom    *left;
        long    size;
        long    pad;  /* must make this structure some multiple of a double */
}Atom, *AtomPtr;
#ifndef UTIL_OPAQUE
void *list_new(IN_T,char *);
void *list_newlh();

void list_old(void *);
void list_oldlh(void *);
#ifndef LL_INLINE
void *list_next(void *);
void *list_prev(void *);
void list_insert_tail(void *,void *);
void list_insert_head(void *,void *);
#else
#ifndef LIST_INLINE
#define LIST_INLINE
#endif
#endif

void *list_remove(void *);
void  list_insert(void *,void *);

void *list_remove_tail(void *);
void *list_remove_head(void *);

void list_clobber_check(void *);
void list_clobber_check_all(IN_T, void *,void *);
IN_T  list_ishead(void *);
#else
void *list_new();
void *list_newlh();

void list_old();
void list_oldlh();

#ifndef LL_INLINE
void *list_next();
void *list_prev();
void list_insert_tail();
void list_insert_head();
#endif

void *list_remove();
void  list_insert();

void *list_remove_tail();
void *list_remove_head();

void list_clobber_check();
void list_clobber_check_all();
IN_T  list_ishead();
#endif
void list_stats();


typedef char *PCHAR;    /* pointer to char */
typedef PCHAR (*FPC)(); /* FPC is a pointer to a func that returns char ptrs */


PSYMTABLE hash_stabinit(IN_T);
IN_T       hash_sdelete();
void      hash_sdump();
//Ptr       hash_slookup();
PSYMBLOCK hash_sinstall(PSYMTABLE table,char *sym,void *p);
Ptr		  hash_slookup(PSYMTABLE table,char *sym);


#endif
#ifdef LL_INLINE
#define list_next(_a_)    ((void *) (( ((struct atom *)_a_) -1)->right+ 1)) 
#define list_prev(_a_)    ((void *) (( ((struct atom *)_a_) -1)->left+ 1)) 
#define list_insert_tail(_a_,_b_)   list_insert(list_prev(_a_),_b_);
#define list_insert_head(_a_,_b_)   list_insert(_a_,_b_);
//#define list_insert(list,obj) {struct atom *_eee_,*_ppp_;_eee_ = ((struct atom *)obj)  - 1;_ppp_ = ((struct atom *)list) - 1; _eee_ ->right = _ppp_->right; _eee_ ->left = _ppp_;(_ppp_->right)->left = _eee_ ;_ppp_->right = _eee_ ; }
#endif
//typedef struct atom Atom,*Atomptr;
