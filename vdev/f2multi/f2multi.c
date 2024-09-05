#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "carto/cartoMemUtils.h"
#include "carto/cartoStrUtils.h"
#include "carto/cartoGtUtils.h"
#include "carto/cartoLsqUtils.h"
#include "carto/cartoVicarProtos.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif


/*  image top to bottom concatenate  A. Zobrist    10/28/99   */

void main44(void)
{
   int i,iinp,iline,isamp,nl,ns,inpcnt,i_unit[48],inl[48],ins[48],dummy,status;
   int fcase,o_unit[48],iband,itemp,outpcnt,option,way,iway,pan,opan,oshade;
   short int **inbuf,**outbuf,**dif,*imtot,inbuftmp;
   float b1,b2,b3,b4,b5,b6,b7,b8,gthresh;
   float ratio_23,ratio_34,ratio_45,ratio_56,ratio_14,ratio_46,ratio_68,ratio_35;
   
   zifmessage("f2multi version Thu Sep  8 2011");
   
   /* get the parms */

   status = zvpcnt("INP",&inpcnt);
   status = zvpcnt("OUT",&outpcnt);
   zvp("OPTION",&option,&dummy);
   zvp("WAY",&way,&dummy);
   zvp("FCASE",&fcase,&dummy);
   zvp("SHADOW",&gthresh,&dummy);
   zvp("PAN",&pan,&dummy);
   if (pan==1&&way==2) zmabend("cannot do two-way in pan case");
   opan = 6*pan;
   oshade = 2*(1-pan);
   
   /* open the inputs and outputs */
   
   for (i=0;i<inpcnt;i++)
      {
      status = zvunit(&i_unit[i],"INP",i+1, NULL);
      status = zvopen(i_unit[i],"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
      zvget(i_unit[i],"NL",&inl[i],"NS",&ins[i], NULL);
      if (inl[i]!=inl[0]||ins[i]!=ins[0]) zmabend("Images must be same size");
      }
   nl = inl[0];
   ns = ins[0];
   
   for (i=0;i<outpcnt;i++)
      {
      status=zvunit(&o_unit[i],"OUT",i+1, NULL);
      status=zvopen(o_unit[i],"U_NL",nl,"U_NS",ns,"U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
      }
   
   /* dynamically allocate the buffers */
   
   mz_alloc2((unsigned char ***)&inbuf,inpcnt,ns,2);
   mz_alloc2((unsigned char ***)&outbuf,outpcnt,ns,2);

   mz_alloc2((unsigned char ***)&dif,8,ns,2);
   mz_alloc1((unsigned char **)&imtot,ns,2);
   
   /* read the inputs calculate per line and output */

   for (iline=0;iline<nl;iline++)
      {
      for (iinp=0;iinp<inpcnt;iinp++)
         {
         status = zvread(i_unit[iinp],inbuf[iinp],"LINE",iline+1,
               "SAMP",1,"NSAMPS",ns, NULL);
         }
      for (isamp=0;isamp<ns;isamp++) outbuf[0][isamp] = 0;
      for (iway=0;iway<way;iway++)
      {
      if (iway==1)
         {
         for (iband=0;iband<8;iband++)
            for (isamp=0;isamp<ns;isamp++)
            {
            inbuftmp = inbuf[iband][isamp];
            inbuf[iband][isamp] = inbuf[iband+8][isamp];
            inbuf[iband+8][isamp] = inbuftmp;
            }
         }
      switch (fcase)
         {
         case 1:  /* testing purposes only */
            for (isamp=0;isamp<ns;isamp++)
               outbuf[0][isamp] = inbuf[0][isamp]+inbuf[1][isamp];
         break;
         case 2:
            for (isamp=0;isamp<ns;isamp++)
               {
               if (pan==0)
                  {
                  itemp = 0;
                  for (iband=0;iband<8;iband++)
                     {
                     dif[iband][isamp] =
                        max(0,min(255,inbuf[iband+8][isamp]-inbuf[iband][isamp]+128));
                     itemp += abs(dif[iband][isamp]-128);
                     }
                  imtot[isamp] = itemp/8;
                  }
               else
                  {
                  imtot[isamp] = abs(inbuf[1][isamp]-inbuf[0][isamp]);
                  if (iline==isamp&&iline%100==0) 
                         printf("i,val %d %d %d\n",iline,inbuf[0][isamp],inbuf[1][isamp]);
                  }
               
               /* ratios, 0.0000037 avoids divide by 0 */
               
               b1 = (float)(inbuf[8-opan][isamp]);
               b2 = (float)(inbuf[9-opan][isamp]);
               b3 = (float)(inbuf[10-opan][isamp]);
               b4 = (float)(inbuf[11-opan][isamp]);
               b5 = (float)(inbuf[12-opan][isamp]);
               b6 = (float)(inbuf[13-opan][isamp]);
               b7 = (float)(inbuf[14-opan][isamp]);
               b8 = (float)(inbuf[15-opan][isamp]);
               ratio_23 = 100.0*((b2-b3)/(b2+b3+0.0000037)+1.0);
               ratio_34 = 100.0*((b3-b4)/(b3+b4+0.0000037)+1.0);
               ratio_45 = 100.0*((b4-b5)/(b4+b5+0.0000037)+1.0);
               ratio_56 = 100.0*((b5-b6)/(b5+b6+0.0000037)+1.0);
               ratio_14 = 100.0*((b1-b4)/(b1+b4+0.0000037)+1.0);
               ratio_46 = 100.0*((b4-b6)/(b4+b6+0.0000037)+1.0);
               ratio_68 = 100.0*((b6-b8)/(b6+b8+0.0000037)+1.0);
               ratio_35 = 100.0*((b3-b5)/(b3+b5+0.0000037)+1.0);
               
               if (option==1)
                  {
                  if (pan==0) for (i=1;i<outpcnt-9;i++) outbuf[i][isamp] = dif[i-1][isamp];
                  outbuf[outpcnt-9][isamp] = imtot[isamp];
                  outbuf[outpcnt-8][isamp] = ratio_23;
                  outbuf[outpcnt-7][isamp] = ratio_34;
                  outbuf[outpcnt-6][isamp] = ratio_45;
                  outbuf[outpcnt-5][isamp] = ratio_56;
                  outbuf[outpcnt-4][isamp] = ratio_14;
                  outbuf[outpcnt-3][isamp] = ratio_46;
                  outbuf[outpcnt-2][isamp] = ratio_68;
                  outbuf[outpcnt-1][isamp] = ratio_35;
                  }
               
               /* See matl note 1 */
               if (inbuf[8-opan][isamp]>677&&inbuf[8-opan][isamp]<697&&
                   inbuf[9-opan][isamp]>673&&inbuf[9-opan][isamp]<693&&
                   inbuf[10-opan][isamp]>685&&inbuf[10-opan][isamp]<705&&
                   inbuf[11-opan][isamp]>685&&inbuf[11-opan][isamp]<705&&
                   inbuf[12-opan][isamp]>682&&inbuf[12-opan][isamp]<702&&
                   inbuf[13-opan][isamp]>678&&inbuf[13-opan][isamp]<698&&
                   inbuf[14-opan][isamp]>677&&inbuf[14-opan][isamp]<697&&
                   inbuf[15-opan][isamp]>669&&inbuf[15-opan][isamp]<689&&
                   dif[0][isamp]>140&&
                   dif[1][isamp]>140&&
                   dif[2][isamp]<128&&
                   dif[3][isamp]<128&&
                   dif[4][isamp]>140&&
                   dif[5][isamp]<128&&
                   dif[6][isamp]>132&&
                   dif[7][isamp]<127&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>32766) outbuf[0][isamp] = 13;
               
               /* See matl note 2 */
               if (inbuf[8-opan][isamp]>354&&inbuf[8-opan][isamp]<499&&
                   inbuf[9-opan][isamp]>353&&inbuf[9-opan][isamp]<467&&
                   inbuf[10-opan][isamp]>335&&inbuf[10-opan][isamp]<421&&
                   inbuf[11-opan][isamp]>302&&inbuf[11-opan][isamp]<398&&
                   inbuf[12-opan][isamp]>326&&inbuf[12-opan][isamp]<394&&
                   inbuf[13-opan][isamp]>285&&inbuf[13-opan][isamp]<379&&
                   inbuf[14-opan][isamp]>338&&inbuf[14-opan][isamp]<438&&
                   inbuf[15-opan][isamp]>329&&inbuf[15-opan][isamp]<483&&
                   ratio_14>99.0&&ratio_14<126.0&&
                   ratio_23>99.0&&ratio_23<110.0&&
                   ratio_34>99.0&&ratio_34<112.0&&
                   ratio_35>99.0&&ratio_35<107.0&&
                   ratio_45>94.0&&ratio_45<104.0&&
                   ratio_46>97.0&&ratio_46<111.0&&
                   ratio_56>97.0&&ratio_56<114.0&&
                   ratio_68>67.0&&ratio_68<102.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>32766) outbuf[0][isamp] = 6;
               
	       /* See matl note 3 */
               if (inbuf[8-opan][isamp]>353&&inbuf[8-opan][isamp]<373&&
                   inbuf[9-opan][isamp]>351&&inbuf[9-opan][isamp]<371&&
                   inbuf[10-opan][isamp]>332&&inbuf[10-opan][isamp]<352&&
                   inbuf[11-opan][isamp]>351&&inbuf[11-opan][isamp]<371&&
                   inbuf[12-opan][isamp]>351&&inbuf[12-opan][isamp]<371&&
                   inbuf[13-opan][isamp]>351&&inbuf[13-opan][isamp]<371&&
                   inbuf[14-opan][isamp]>351&&inbuf[14-opan][isamp]<371&&
                   inbuf[15-opan][isamp]>351&&inbuf[15-opan][isamp]<371&&
                   imtot[isamp]>14) outbuf[0][isamp] = 0;/*was 2*/

	       /* See matl note 4 */
               if (inbuf[8-opan][isamp]>440.0&&inbuf[8-opan][isamp]<576.0&&
                   inbuf[9-opan][isamp]>417.0&&inbuf[9-opan][isamp]<525.0&&
                   inbuf[10-opan][isamp]>366.0&&inbuf[10-opan][isamp]<506.0&&
                   inbuf[11-opan][isamp]>304.0&&inbuf[11-opan][isamp]<462.0&&
                   inbuf[12-opan][isamp]>267.0&&inbuf[12-opan][isamp]<425.0&&
                   inbuf[13-opan][isamp]>229.0&&inbuf[13-opan][isamp]<391.0&&
                   inbuf[14-opan][isamp]>290.0&&inbuf[14-opan][isamp]<402.0&&
                   inbuf[15-opan][isamp]>283.0&&inbuf[15-opan][isamp]<439.0&&
                   ratio_14>105.0&&ratio_14<128.0&&
                   ratio_23>98.0&&ratio_23<109.0&&
                   ratio_34>101.0&&ratio_34<115.0&&
                   ratio_35>103.0&&ratio_35<114.0&&
                   ratio_45>104.0&&ratio_45<118.0&&
                   ratio_46>106.0&&ratio_46<122.0&&
                   ratio_56>95.0&&ratio_56<123.0&&
                   ratio_68>81.0&&ratio_68<98.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>30) outbuf[0][isamp] = 10;
               
               /* See matl note 5 */
               if (inbuf[8-opan][isamp]>373.0&&inbuf[8-opan][isamp]<487.0&&
                   inbuf[9-opan][isamp]>356.0&&inbuf[9-opan][isamp]<455.0&&
                   inbuf[10-opan][isamp]>205.0&&inbuf[10-opan][isamp]<335.0&&
                   inbuf[11-opan][isamp]>73.0&&inbuf[11-opan][isamp]<276&&
                   inbuf[12-opan][isamp]>215.0&&inbuf[12-opan][isamp]<341.0&&
                   inbuf[13-opan][isamp]>50.0&&inbuf[13-opan][isamp]<258.0&&
                   inbuf[14-opan][isamp]>319.0&&inbuf[14-opan][isamp]<454.0&&
                   inbuf[15-opan][isamp]>354.0&&inbuf[15-opan][isamp]<536.0&&
                   ratio_14>106.0&&ratio_14<183.0&&
                   ratio_23>103.0&&ratio_23<142.0&&
                   ratio_34>103.0&&ratio_34<168.0&&
                   ratio_35>75.0&&ratio_35<118.0&&
                   ratio_45>30.0&&ratio_45<97.0&&
                   ratio_46>95.0&&ratio_46<172.0&&
                   ratio_56>105.0&&ratio_56<194.0&&
                   ratio_68>3.0&&ratio_68<94.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>35) outbuf[0][isamp] = 1;
               
	       /* See matl note 6 */
               if (inbuf[8-opan][isamp]>820.0&&inbuf[8-opan][isamp]<1032.0&&
                   inbuf[9-opan][isamp]>730.0&&inbuf[9-opan][isamp]<902.0&&
                   inbuf[10-opan][isamp]>880.0&&inbuf[10-opan][isamp]<1106.0&&
                   inbuf[11-opan][isamp]>981.0&&inbuf[11-opan][isamp]<1221.0&&
                   inbuf[13-opan][isamp]>936.0&&inbuf[13-opan][isamp]<1162.0&&
                   inbuf[15-opan][isamp]>891.0&&inbuf[15-opan][isamp]<1111.0&&
                   ratio_14>89.0&&ratio_14<112.0&&
                   ratio_23>86.0&&ratio_23<106.0&&
                   ratio_34>91.0&&ratio_34<103.0&&
                   ratio_35>97.0&&ratio_35<117.0&&
                   ratio_45>94.0&&ratio_45<123.0&&
                   ratio_46>95.0&&ratio_46<107.0&&
                   ratio_56>80.0&&ratio_56<109.0&&
                   ratio_68>96.0&&ratio_68<108.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>52) outbuf[0][isamp] = 4;

               /* See matl note 7 */
               if (inbuf[8-opan][isamp]>697&&inbuf[8-opan][isamp]<965&&
                   inbuf[9-opan][isamp]>639&&inbuf[9-opan][isamp]<941&&
                   inbuf[10-opan][isamp]>732&&inbuf[10-opan][isamp]<965&&
                   inbuf[11-opan][isamp]>815&&inbuf[11-opan][isamp]<1061&&
                   inbuf[13-opan][isamp]>799&&inbuf[13-opan][isamp]<1005&&
                   inbuf[15-opan][isamp]>802&&inbuf[15-opan][isamp]<990&&
                   ratio_14>87.0&&ratio_14<101.0&&     
                   ratio_23>89.0&&ratio_23<98.5&&     
                   ratio_34>89.0&&ratio_34<105.5&&     
                   ratio_35>98.0&&ratio_35<112.0&&     
                   ratio_45>102.0&&ratio_45<119.0&&     
                   ratio_46>91.0&&ratio_46<106.0&&     
                   ratio_56>82.0&&ratio_56<101.0&&     
                   ratio_68>97.0&&ratio_68<102.0&&     
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>40)
                         {
                         printf("inbuf[8-opan][isamp] %d\n",inbuf[8-opan][isamp]);
                         printf("inbuf[15-opan][isamp] %d\n",inbuf[15-opan][isamp]);
                         printf("inbuf[15-opan][isamp] %d\n",inbuf[15-opan][isamp]);
                         outbuf[0][isamp] = 12;
                         }
               
               /* See matl note 8 */
               if (inbuf[8-opan][isamp]>632&&inbuf[8-opan][isamp]<762&&
                   inbuf[9-opan][isamp]>599&&inbuf[9-opan][isamp]<689&&
                   inbuf[10-opan][isamp]>706&&inbuf[10-opan][isamp]<846&&
                   inbuf[11-opan][isamp]>783&&inbuf[11-opan][isamp]<971&&
                   inbuf[13-opan][isamp]>774&&inbuf[13-opan][isamp]<952&&
                   inbuf[15-opan][isamp]>769&&inbuf[15-opan][isamp]<921&&
                   ratio_14>87.0&&ratio_14<92.0&&
                   ratio_23>85.0&&ratio_23<94.0&&
                   ratio_34>90.0&&ratio_34<97.0&&
                   ratio_35>98.0&&ratio_35<111.0&&
                   ratio_45>106.0&&ratio_45<118.0&&
                   ratio_46>98.0&&ratio_46<102.0&&
                   ratio_56>82.0&&ratio_56<95.0&&
                   ratio_68>99.0&&ratio_68<103.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>50) outbuf[0][isamp] = 11;
             
	       /* See matl note 9 */
               if (inbuf[8-opan][isamp]>588.0&&inbuf[8-opan][isamp]<797.0&&
                   inbuf[9-opan][isamp]>572.0&&inbuf[9-opan][isamp]<704.0&&
                   inbuf[10-opan][isamp]>627.0&&inbuf[10-opan][isamp]<824.0&&
                   inbuf[11-opan][isamp]>669.0&&inbuf[11-opan][isamp]<893.0&&
                   inbuf[12-opan][isamp]>566.0&&inbuf[12-opan][isamp]<664.0&&
                   inbuf[13-opan][isamp]>669.0&&inbuf[13-opan][isamp]<859.0&&
                   inbuf[14-opan][isamp]>556.0&&inbuf[14-opan][isamp]<659.0&&
                   inbuf[15-opan][isamp]>624.0&&inbuf[15-opan][isamp]<692.0&&
                   ratio_14>90.0&&ratio_14<101.0&&
                   ratio_23>90.0&&ratio_23<97.0&&
                   ratio_34>93.0&&ratio_34<102.0&&
                   ratio_35>98.0&&ratio_35<115.0&&
                   ratio_45>102.0&&ratio_45<121.0&&
                   ratio_46>98.0&&ratio_46<107.0&&
                   ratio_56>84.0&&ratio_56<101.0&&
                   ratio_68>100.0&&ratio_68<107.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>62) outbuf[0][isamp] = 2;
               
               /* See matl note 10 */
               if (inbuf[8-opan][isamp]>430.0&&inbuf[8-opan][isamp]<644.0&&
                   inbuf[9-opan][isamp]>427.0&&inbuf[9-opan][isamp]<599.0&&
                   inbuf[10-opan][isamp]>416.0&&inbuf[10-opan][isamp]<678.0&&
                   inbuf[11-opan][isamp]>413.0&&inbuf[11-opan][isamp]<733.0&&
                   inbuf[12-opan][isamp]>416.0&&inbuf[12-opan][isamp]<602.0&&
                   inbuf[13-opan][isamp]>415.0&&inbuf[13-opan][isamp]<713.0&&
                   inbuf[14-opan][isamp]>419.0&&inbuf[14-opan][isamp]<611.0&&
                   inbuf[15-opan][isamp]>422.0&&inbuf[15-opan][isamp]<704.0&&
                   ratio_14>87.0&&ratio_14<94.0&&
                   ratio_23>89.0&&ratio_23<95.0&&
                   ratio_34>89.0&&ratio_34<99.0&&
                   ratio_35>98.0&&ratio_35<112.0&&
                   ratio_45>107.0&&ratio_45<119.0&&
                   ratio_46>97.0&&ratio_46<107.0&&
                   ratio_56>82.0&&ratio_56<101.0&&
                   ratio_68>97.0&&ratio_68<102.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>35) outbuf[0][isamp] = 3;
               
               /* See matl note 11 */
               if (inbuf[8-opan][isamp]>442&&inbuf[8-opan][isamp]<544&&
                   inbuf[9-opan][isamp]>436&&inbuf[9-opan][isamp]<532&&
                   inbuf[10-opan][isamp]>458&&inbuf[10-opan][isamp]<592&&
                   inbuf[11-opan][isamp]>477&&inbuf[11-opan][isamp]<623&&
                   inbuf[12-opan][isamp]>446&&inbuf[12-opan][isamp]<550&&
                   inbuf[13-opan][isamp]>479&&inbuf[13-opan][isamp]<611&&
                   inbuf[14-opan][isamp]>448&&inbuf[14-opan][isamp]<558&&
                   inbuf[15-opan][isamp]>479&&inbuf[15-opan][isamp]<597&&
                   ratio_14>91.0&&ratio_14<100.5&&
                   ratio_23>92.0&&ratio_23<99.0&&
                   ratio_34>96.0&&ratio_34<100.0&&
                   ratio_35>100.0&&ratio_35<106.0&&
                   ratio_45>97.5&&ratio_45<109.0&&
                   ratio_46>99.0&&ratio_46<102.0&&
                   ratio_56>93.0&&ratio_56<100.5&&
                   ratio_68>96.5&&ratio_68<103.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>25) outbuf[0][isamp] = 7;
               
               /* See matl note 12 */
               if (inbuf[8-opan][isamp]>421&&inbuf[8-opan][isamp]<489&&
                   inbuf[9-opan][isamp]>409&&inbuf[9-opan][isamp]<485&&
                   inbuf[10-opan][isamp]>422&&inbuf[10-opan][isamp]<518&&
                   inbuf[11-opan][isamp]>445&&inbuf[11-opan][isamp]<537&&
                   inbuf[12-opan][isamp]>416&&inbuf[12-opan][isamp]<486&&
                   inbuf[13-opan][isamp]>435&&inbuf[13-opan][isamp]<529&&
                   inbuf[14-opan][isamp]>402&&inbuf[14-opan][isamp]<480&&
                   inbuf[15-opan][isamp]>426&&inbuf[15-opan][isamp]<520&&
                   ratio_14>95.0&&ratio_14<100.0&&
                   ratio_23>95.0&&ratio_23<100.0&&
                   ratio_34>96.0&&ratio_34<101.0&&
                   ratio_35>99.0&&ratio_35<104.0&&
                   ratio_45>101.0&&ratio_45<107.0&&
                   ratio_46>100.0&&ratio_46<103.0&&
                   ratio_56>94.0&&ratio_56<99.0&&
                   ratio_68>98.0&&ratio_68<104.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>50) outbuf[0][isamp] = 8;
               
               /* See matl note 13 */
               if (inbuf[8-opan][isamp]>339&&inbuf[8-opan][isamp]<395&&
                   inbuf[9-opan][isamp]>499&&inbuf[9-opan][isamp]<589&&
                   inbuf[10-opan][isamp]>662&&inbuf[10-opan][isamp]<896&&
                   inbuf[11-opan][isamp]>728&&inbuf[11-opan][isamp]<992&&
                   inbuf[13-opan][isamp]>709&&inbuf[13-opan][isamp]<953&&
                   inbuf[15-opan][isamp]>730&&inbuf[15-opan][isamp]<952&&
                   ratio_14>49.0&&ratio_14<79.0&&
                   ratio_23>75.0&&ratio_23<91.0&&
                   ratio_34>90.0&&ratio_34<99.0&&
                   ratio_35>100.0&&ratio_35<115.0&&
                   ratio_45>103.0&&ratio_45<119.0&&
                   ratio_46>93.0&&ratio_46<109.0&&
                   ratio_56>83.0&&ratio_56<99.0&&
                   ratio_68>96.0&&ratio_68<102.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>25) outbuf[0][isamp] = 9;
               
               /* See matl note 14 */
               if (inbuf[8-opan][isamp]>511&&inbuf[8-opan][isamp]<671&&
                   inbuf[9-opan][isamp]>511&&inbuf[9-opan][isamp]<647&&
                   inbuf[10-opan][isamp]>575&&inbuf[10-opan][isamp]<781&&
                   inbuf[11-opan][isamp]>647&&inbuf[11-opan][isamp]<873&&
                   inbuf[12-opan][isamp]>543&&inbuf[12-opan][isamp]<721&&
                   inbuf[13-opan][isamp]>658&&inbuf[13-opan][isamp]<865&&
                   inbuf[14-opan][isamp]>573&&inbuf[14-opan][isamp]<741&&
                   inbuf[15-opan][isamp]>680&&inbuf[15-opan][isamp]<861&&
                   ratio_14>84.0&&ratio_14<93.0&&
                   ratio_23>87.0&&ratio_23<99.0&&
                   ratio_34>90.0&&ratio_34<99.0&&
                   ratio_35>96.0&&ratio_35<109.0&&
                   ratio_45>98.0&&ratio_45<117.0&&
                   ratio_46>95.0&&ratio_46<103.0&&
                   ratio_56>83.0&&ratio_56<101.0&&
                   ratio_68>93.0&&ratio_68<104.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>65) outbuf[0][isamp] = 5;
                }
         break;
         case 3:
            for (isamp=0;isamp<ns;isamp++)
               {
               outbuf[0][isamp] = (inbuf[0][isamp]<0.5)*inbuf[4][isamp]*0.4+
                                  (inbuf[0][isamp]>0.5)*inbuf[1][isamp];
               outbuf[1][isamp] = (inbuf[0][isamp]<0.5)*inbuf[4][isamp]*0.4+
                                  (inbuf[0][isamp]>0.5)*inbuf[2][isamp];
               outbuf[2][isamp] = (inbuf[0][isamp]<0.5)*inbuf[4][isamp]*0.4+
                                  (inbuf[0][isamp]>0.5)*inbuf[3][isamp];
               }
         break;
         case 4:
            for (isamp=0;isamp<ns;isamp++)
               {
               if (pan==0)
                  {
                  itemp = 0;
                  for (iband=0;iband<8;iband++)
                     {
                     dif[iband][isamp] =
                        max(0,min(255,inbuf[iband+8][isamp]-inbuf[iband][isamp]+128));
                     itemp += abs(dif[iband][isamp]-128);
                     }
                  imtot[isamp] = itemp/8;
                  }
               else
                  {
                  imtot[isamp] = abs(inbuf[1]-inbuf[0]);
                  }
               
               /* ratios, 0.0000037 avoids divide by 0 */
               
               b1 = (float)(inbuf[8-opan][isamp]);
               b2 = (float)(inbuf[9-opan][isamp]);
               b3 = (float)(inbuf[10-opan][isamp]);
               b4 = (float)(inbuf[11-opan][isamp]);
               b5 = (float)(inbuf[12-opan][isamp]);
               b6 = (float)(inbuf[13-opan][isamp]);
               b7 = (float)(inbuf[14-opan][isamp]);
               b8 = (float)(inbuf[15-opan][isamp]);
               ratio_23 = 100.0*((b2-b3)/(b2+b3+0.0000037)+1.0);
               ratio_34 = 100.0*((b3-b4)/(b3+b4+0.0000037)+1.0);
               ratio_45 = 100.0*((b4-b5)/(b4+b5+0.0000037)+1.0);
               ratio_56 = 100.0*((b5-b6)/(b5+b6+0.0000037)+1.0);
               ratio_14 = 100.0*((b1-b4)/(b1+b4+0.0000037)+1.0);
               ratio_46 = 100.0*((b4-b6)/(b4+b6+0.0000037)+1.0);
               ratio_68 = 100.0*((b6-b8)/(b6+b8+0.0000037)+1.0);
               ratio_35 = 100.0*((b3-b5)/(b3+b5+0.0000037)+1.0);
               
               if (option==1)
                  {
                  if (pan==0) for (i=1;i<outpcnt-9;i++) outbuf[i][isamp] = dif[i-1][isamp];
                  outbuf[outpcnt-9][isamp] = imtot[isamp];
                  outbuf[outpcnt-8][isamp] = ratio_23;
                  outbuf[outpcnt-7][isamp] = ratio_34;
                  outbuf[outpcnt-6][isamp] = ratio_45;
                  outbuf[outpcnt-5][isamp] = ratio_56;
                  outbuf[outpcnt-4][isamp] = ratio_14;
                  outbuf[outpcnt-3][isamp] = ratio_46;
                  outbuf[outpcnt-2][isamp] = ratio_68;
                  outbuf[outpcnt-1][isamp] = ratio_35;
                  }
               
               /* See matl note 1 */
               if (inbuf[8-opan][isamp]>677&&inbuf[8-opan][isamp]<697&&
                   inbuf[9-opan][isamp]>673&&inbuf[9-opan][isamp]<693&&
                   inbuf[10-opan][isamp]>685&&inbuf[10-opan][isamp]<705&&
                   inbuf[11-opan][isamp]>685&&inbuf[11-opan][isamp]<705&&
                   inbuf[12-opan][isamp]>682&&inbuf[12-opan][isamp]<702&&
                   inbuf[13-opan][isamp]>678&&inbuf[13-opan][isamp]<698&&
                   inbuf[14-opan][isamp]>677&&inbuf[14-opan][isamp]<697&&
                   inbuf[15-opan][isamp]>669&&inbuf[15-opan][isamp]<689&&
                   dif[0][isamp]>140&&
                   dif[1][isamp]>140&&
                   dif[2][isamp]<128&&
                   dif[3][isamp]<128&&
                   dif[4][isamp]>140&&
                   dif[5][isamp]<128&&
                   dif[6][isamp]>132&&
                   dif[7][isamp]<127&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>45) outbuf[0][isamp] = 13;
               
               /* See matl note 2 */
               if (inbuf[8-opan][isamp]>354&&inbuf[8-opan][isamp]<499&&
                   inbuf[9-opan][isamp]>353&&inbuf[9-opan][isamp]<467&&
                   inbuf[10-opan][isamp]>335&&inbuf[10-opan][isamp]<421&&
                   inbuf[11-opan][isamp]>302&&inbuf[11-opan][isamp]<398&&
                   inbuf[12-opan][isamp]>326&&inbuf[12-opan][isamp]<394&&
                   inbuf[13-opan][isamp]>285&&inbuf[13-opan][isamp]<379&&
                   inbuf[14-opan][isamp]>338&&inbuf[14-opan][isamp]<438&&
                   inbuf[15-opan][isamp]>329&&inbuf[15-opan][isamp]<483&&
                   ratio_14>99.0&&ratio_14<126.0&&
                   ratio_23>99.0&&ratio_23<110.0&&
                   ratio_34>99.0&&ratio_34<112.0&&
                   ratio_35>99.0&&ratio_35<107.0&&
                   ratio_45>94.0&&ratio_45<104.0&&
                   ratio_46>97.0&&ratio_46<111.0&&
                   ratio_56>97.0&&ratio_56<114.0&&
                   ratio_68>67.0&&ratio_68<102.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>40) outbuf[0][isamp] = 6;
               
	       /* See matl note 3 */
               if (inbuf[8-opan][isamp]>353&&inbuf[8-opan][isamp]<373&&
                   inbuf[9-opan][isamp]>351&&inbuf[9-opan][isamp]<371&&
                   inbuf[10-opan][isamp]>332&&inbuf[10-opan][isamp]<352&&
                   inbuf[11-opan][isamp]>351&&inbuf[11-opan][isamp]<371&&
                   inbuf[12-opan][isamp]>351&&inbuf[12-opan][isamp]<371&&
                   inbuf[13-opan][isamp]>351&&inbuf[13-opan][isamp]<371&&
                   inbuf[14-opan][isamp]>351&&inbuf[14-opan][isamp]<371&&
                   inbuf[15-opan][isamp]>351&&inbuf[15-opan][isamp]<371&&
                   imtot[isamp]>14) outbuf[0][isamp] = 0;/*was 2*/

	       /* See matl note 4 */
               if (inbuf[8-opan][isamp]>440.0&&inbuf[8-opan][isamp]<576.0&&
                   inbuf[9-opan][isamp]>417.0&&inbuf[9-opan][isamp]<525.0&&
                   inbuf[10-opan][isamp]>366.0&&inbuf[10-opan][isamp]<506.0&&
                   inbuf[11-opan][isamp]>304.0&&inbuf[11-opan][isamp]<462.0&&
                   inbuf[12-opan][isamp]>267.0&&inbuf[12-opan][isamp]<425.0&&
                   inbuf[13-opan][isamp]>229.0&&inbuf[13-opan][isamp]<391.0&&
                   inbuf[14-opan][isamp]>290.0&&inbuf[14-opan][isamp]<402.0&&
                   inbuf[15-opan][isamp]>283.0&&inbuf[15-opan][isamp]<439.0&&
                   ratio_14>105.0&&ratio_14<128.0&&
                   ratio_23>98.0&&ratio_23<109.0&&
                   ratio_34>101.0&&ratio_34<115.0&&
                   ratio_35>103.0&&ratio_35<114.0&&
                   ratio_45>104.0&&ratio_45<118.0&&
                   ratio_46>106.0&&ratio_46<122.0&&
                   ratio_56>95.0&&ratio_56<123.0&&
                   ratio_68>81.0&&ratio_68<98.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>25) outbuf[0][isamp] = 10;
               
               /* See matl note 5 */
               if (inbuf[8-opan][isamp]>373.0&&inbuf[8-opan][isamp]<487.0&&
                   inbuf[9-opan][isamp]>356.0&&inbuf[9-opan][isamp]<455.0&&
                   inbuf[10-opan][isamp]>205.0&&inbuf[10-opan][isamp]<335.0&&
                   inbuf[11-opan][isamp]>73.0&&inbuf[11-opan][isamp]<276&&
                   inbuf[12-opan][isamp]>215.0&&inbuf[12-opan][isamp]<341.0&&
                   inbuf[13-opan][isamp]>50.0&&inbuf[13-opan][isamp]<258.0&&
                   inbuf[14-opan][isamp]>319.0&&inbuf[14-opan][isamp]<454.0&&
                   inbuf[15-opan][isamp]>354.0&&inbuf[15-opan][isamp]<536.0&&
                   ratio_14>106.0&&ratio_14<183.0&&
                   ratio_23>103.0&&ratio_23<142.0&&
                   ratio_34>103.0&&ratio_34<168.0&&
                   ratio_35>75.0&&ratio_35<118.0&&
                   ratio_45>30.0&&ratio_45<97.0&&
                   ratio_46>95.0&&ratio_46<172.0&&
                   ratio_56>105.0&&ratio_56<194.0&&
                   ratio_68>3.0&&ratio_68<94.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>32) outbuf[0][isamp] = 1;
               
	       /* See matl note 6 */
               if (inbuf[8-opan][isamp]>820.0&&inbuf[8-opan][isamp]<1032.0&&
                   inbuf[9-opan][isamp]>730.0&&inbuf[9-opan][isamp]<902.0&&
                   inbuf[10-opan][isamp]>880.0&&inbuf[10-opan][isamp]<1106.0&&
                   inbuf[11-opan][isamp]>981.0&&inbuf[11-opan][isamp]<1221.0&&
                   inbuf[13-opan][isamp]>936.0&&inbuf[13-opan][isamp]<1162.0&&
                   inbuf[15-opan][isamp]>891.0&&inbuf[15-opan][isamp]<1111.0&&
                   ratio_14>89.0&&ratio_14<112.0&&
                   ratio_23>86.0&&ratio_23<106.0&&
                   ratio_34>91.0&&ratio_34<103.0&&
                   ratio_35>97.0&&ratio_35<117.0&&
                   ratio_45>94.0&&ratio_45<123.0&&
                   ratio_46>95.0&&ratio_46<107.0&&
                   ratio_56>80.0&&ratio_56<109.0&&
                   ratio_68>96.0&&ratio_68<108.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>61) outbuf[0][isamp] = 4;

               /* See matl note 7 */
               if (inbuf[8-opan][isamp]>697&&inbuf[8-opan][isamp]<965&&
                   inbuf[9-opan][isamp]>639&&inbuf[9-opan][isamp]<941&&
                   inbuf[10-opan][isamp]>732&&inbuf[10-opan][isamp]<965&&
                   inbuf[11-opan][isamp]>815&&inbuf[11-opan][isamp]<1061&&
                   inbuf[13-opan][isamp]>799&&inbuf[13-opan][isamp]<1005&&
                   inbuf[15-opan][isamp]>802&&inbuf[15-opan][isamp]<990&&
                   ratio_14>87.0&&ratio_14<101.0&&     
                   ratio_23>89.0&&ratio_23<98.5&&     
                   ratio_34>89.0&&ratio_34<105.5&&     
                   ratio_35>98.0&&ratio_35<112.0&&     
                   ratio_45>102.0&&ratio_45<119.0&&     
                   ratio_46>91.0&&ratio_46<106.0&&     
                   ratio_56>82.0&&ratio_56<101.0&&     
                   ratio_68>97.0&&ratio_68<102.0&&     
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>40) outbuf[0][isamp] = 12;
               
               /* See matl note 8 */
               if (inbuf[8-opan][isamp]>632&&inbuf[8-opan][isamp]<762&&
                   inbuf[9-opan][isamp]>599&&inbuf[9-opan][isamp]<689&&
                   inbuf[10-opan][isamp]>706&&inbuf[10-opan][isamp]<846&&
                   inbuf[11-opan][isamp]>783&&inbuf[11-opan][isamp]<971&&
                   inbuf[13-opan][isamp]>774&&inbuf[13-opan][isamp]<952&&
                   inbuf[15-opan][isamp]>769&&inbuf[15-opan][isamp]<921&&
                   ratio_14>87.0&&ratio_14<92.0&&
                   ratio_23>85.0&&ratio_23<94.0&&
                   ratio_34>90.0&&ratio_34<97.0&&
                   ratio_35>98.0&&ratio_35<111.0&&
                   ratio_45>106.0&&ratio_45<118.0&&
                   ratio_46>98.0&&ratio_46<102.0&&
                   ratio_56>82.0&&ratio_56<95.0&&
                   ratio_68>99.0&&ratio_68<103.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>35) outbuf[0][isamp] = 11;
             
	       /* See matl note 9 */
               if (inbuf[8-opan][isamp]>588.0&&inbuf[8-opan][isamp]<797.0&&
                   inbuf[9-opan][isamp]>572.0&&inbuf[9-opan][isamp]<704.0&&
                   inbuf[10-opan][isamp]>627.0&&inbuf[10-opan][isamp]<824.0&&
                   inbuf[11-opan][isamp]>669.0&&inbuf[11-opan][isamp]<893.0&&
                   inbuf[12-opan][isamp]>566.0&&inbuf[12-opan][isamp]<664.0&&
                   inbuf[13-opan][isamp]>669.0&&inbuf[13-opan][isamp]<859.0&&
                   inbuf[14-opan][isamp]>556.0&&inbuf[14-opan][isamp]<659.0&&
                   inbuf[15-opan][isamp]>624.0&&inbuf[15-opan][isamp]<692.0&&
                   ratio_14>90.0&&ratio_14<101.0&&
                   ratio_23>90.0&&ratio_23<97.0&&
                   ratio_34>93.0&&ratio_34<102.0&&
                   ratio_35>98.0&&ratio_35<115.0&&
                   ratio_45>102.0&&ratio_45<121.0&&
                   ratio_46>98.0&&ratio_46<107.0&&
                   ratio_56>84.0&&ratio_56<101.0&&
                   ratio_68>100.0&&ratio_68<107.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>62) outbuf[0][isamp] = 2;
               
               /* See matl note 10 */
               if (inbuf[8-opan][isamp]>430.0&&inbuf[8-opan][isamp]<644.0&&
                   inbuf[9-opan][isamp]>427.0&&inbuf[9-opan][isamp]<599.0&&
                   inbuf[10-opan][isamp]>416.0&&inbuf[10-opan][isamp]<678.0&&
                   inbuf[11-opan][isamp]>413.0&&inbuf[11-opan][isamp]<733.0&&
                   inbuf[12-opan][isamp]>416.0&&inbuf[12-opan][isamp]<602.0&&
                   inbuf[13-opan][isamp]>415.0&&inbuf[13-opan][isamp]<713.0&&
                   inbuf[14-opan][isamp]>419.0&&inbuf[14-opan][isamp]<611.0&&
                   inbuf[15-opan][isamp]>422.0&&inbuf[15-opan][isamp]<704.0&&
                   ratio_14>87.0&&ratio_14<94.0&&
                   ratio_23>89.0&&ratio_23<95.0&&
                   ratio_34>89.0&&ratio_34<99.0&&
                   ratio_35>98.0&&ratio_35<112.0&&
                   ratio_45>107.0&&ratio_45<119.0&&
                   ratio_46>97.0&&ratio_46<107.0&&
                   ratio_56>82.0&&ratio_56<101.0&&
                   ratio_68>97.0&&ratio_68<102.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>35) outbuf[0][isamp] = 3;
               
               /* See matl note 11 */
               if (inbuf[8-opan][isamp]>442&&inbuf[8-opan][isamp]<544&&
                   inbuf[9-opan][isamp]>436&&inbuf[9-opan][isamp]<532&&
                   inbuf[10-opan][isamp]>458&&inbuf[10-opan][isamp]<592&&
                   inbuf[11-opan][isamp]>477&&inbuf[11-opan][isamp]<623&&
                   inbuf[12-opan][isamp]>446&&inbuf[12-opan][isamp]<550&&
                   inbuf[13-opan][isamp]>479&&inbuf[13-opan][isamp]<611&&
                   inbuf[14-opan][isamp]>448&&inbuf[14-opan][isamp]<558&&
                   inbuf[15-opan][isamp]>479&&inbuf[15-opan][isamp]<597&&
                   ratio_14>91.0&&ratio_14<100.5&&
                   ratio_23>92.0&&ratio_23<99.0&&
                   ratio_34>96.0&&ratio_34<100.0&&
                   ratio_35>100.0&&ratio_35<106.0&&
                   ratio_45>97.5&&ratio_45<109.0&&
                   ratio_46>99.0&&ratio_46<102.0&&
                   ratio_56>93.0&&ratio_56<100.5&&
                   ratio_68>96.5&&ratio_68<103.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>25) outbuf[0][isamp] = 7;
               
               /* See matl note 12 */
               if (inbuf[8-opan][isamp]>421&&inbuf[8-opan][isamp]<489&&
                   inbuf[9-opan][isamp]>409&&inbuf[9-opan][isamp]<485&&
                   inbuf[10-opan][isamp]>422&&inbuf[10-opan][isamp]<518&&
                   inbuf[11-opan][isamp]>445&&inbuf[11-opan][isamp]<537&&
                   inbuf[12-opan][isamp]>416&&inbuf[12-opan][isamp]<486&&
                   inbuf[13-opan][isamp]>435&&inbuf[13-opan][isamp]<529&&
                   inbuf[14-opan][isamp]>402&&inbuf[14-opan][isamp]<480&&
                   inbuf[15-opan][isamp]>426&&inbuf[15-opan][isamp]<520&&
                   ratio_14>95.0&&ratio_14<100.0&&
                   ratio_23>95.0&&ratio_23<100.0&&
                   ratio_34>96.0&&ratio_34<101.0&&
                   ratio_35>99.0&&ratio_35<104.0&&
                   ratio_45>101.0&&ratio_45<107.0&&
                   ratio_46>100.0&&ratio_46<103.0&&
                   ratio_56>94.0&&ratio_56<99.0&&
                   ratio_68>98.0&&ratio_68<104.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>50) outbuf[0][isamp] = 8;
               
               /* See matl note 13 */
               if (inbuf[8-opan][isamp]>339&&inbuf[8-opan][isamp]<395&&
                   inbuf[9-opan][isamp]>499&&inbuf[9-opan][isamp]<589&&
                   inbuf[10-opan][isamp]>662&&inbuf[10-opan][isamp]<896&&
                   inbuf[11-opan][isamp]>728&&inbuf[11-opan][isamp]<992&&
                   inbuf[13-opan][isamp]>709&&inbuf[13-opan][isamp]<953&&
                   inbuf[15-opan][isamp]>730&&inbuf[15-opan][isamp]<952&&
                   ratio_14>49.0&&ratio_14<79.0&&
                   ratio_23>75.0&&ratio_23<91.0&&
                   ratio_34>90.0&&ratio_34<99.0&&
                   ratio_35>100.0&&ratio_35<115.0&&
                   ratio_45>103.0&&ratio_45<119.0&&
                   ratio_46>93.0&&ratio_46<109.0&&
                   ratio_56>83.0&&ratio_56<99.0&&
                   ratio_68>96.0&&ratio_68<102.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>25) outbuf[0][isamp] = 9;
               
               /* See matl note 14 */
               if (inbuf[8-opan][isamp]>511&&inbuf[8-opan][isamp]<671&&
                   inbuf[9-opan][isamp]>511&&inbuf[9-opan][isamp]<647&&
                   inbuf[10-opan][isamp]>575&&inbuf[10-opan][isamp]<781&&
                   inbuf[11-opan][isamp]>647&&inbuf[11-opan][isamp]<873&&
                   inbuf[12-opan][isamp]>543&&inbuf[12-opan][isamp]<721&&
                   inbuf[13-opan][isamp]>658&&inbuf[13-opan][isamp]<865&&
                   inbuf[14-opan][isamp]>573&&inbuf[14-opan][isamp]<741&&
                   inbuf[15-opan][isamp]>680&&inbuf[15-opan][isamp]<861&&
                   ratio_14>84.0&&ratio_14<93.0&&
                   ratio_23>87.0&&ratio_23<99.0&&
                   ratio_34>90.0&&ratio_34<99.0&&
                   ratio_35>96.0&&ratio_35<109.0&&
                   ratio_45>98.0&&ratio_45<117.0&&
                   ratio_46>95.0&&ratio_46<103.0&&
                   ratio_56>83.0&&ratio_56<101.0&&
                   ratio_68>93.0&&ratio_68<104.0&&
                   inbuf[oshade][isamp]>gthresh&&
                   imtot[isamp]>55) outbuf[0][isamp] = 5;
                }
         break;
         }
      }
      for (i=0;i<outpcnt;i++)
         zvwrit(o_unit[i],outbuf[i],"LINE",iline+1,"SAMP",1,"NSAMPS",ns, NULL);
      }
   
   for (i=0;i<inpcnt;i++) zvclose(i_unit[i], NULL);
   for (i=0;i<outpcnt;i++) zvclose(o_unit[i], NULL);
   return;
}
