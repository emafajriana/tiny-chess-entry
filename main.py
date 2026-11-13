import os,json,random,urllib.request as U
random.seed(31337)
Z=[random.getrandbits(64)for _ in range(1580)]
V=(0,100,320,330,500,950,0)
PT="KKKKUUUUMMOQLLMPKKKOLJIKLMMGKKKKACEECGKKEKMNELNOEKNOELMNCGKLACEEGIIIIKKKIKLMILLMIKMMIMMMILKKGIIIKKKKLMMMJKKKJKKKJKKKJKKKJKKKKKKLGIIJIKKKIKLLJKLLKKLLILLLIKLKGIIJECCAECCAECCAECCAGEECIGGGOOKKOQMK"
D8=(15,17,-15,-17,1,-1,16,-16);NN=(33,31,18,14,-14,-18,-31,-33);LD=(NN,D8[:4],D8[4:],D8,D8)
CR={0:13,7:14,4:12,112:7,119:11,116:3}
INF=32000;MATE=30000;BUD=36000;B=abs;I=enumerate
class X(Exception):pass
def F(f):
 a=f.split();b=[0]*128
 for y,row in I(a[0].split('/')):
  x=0
  for c in row:
   if c<'9':x+=int(c);continue
   b[(7-y)*16+x]=' pnbrqk'.find(c.lower())*(1-2*(c>'Z'));x+=1
 return b,1-2*(a[1]=='b'),sum((c in a[2])<<i for i,c in I('KQkq')),-1 if a[3]=='-'else(int(a[3][1])-1)*16+ord(a[3][0])-97
def A(b,s,c):
 for d in(-15*c,-17*c)+NN:
  t=s+d
  if not t&136 and b[t]==c*(2 if d in NN else 1):return 1
 for i,d in I(D8):
  t=s+d
  while not t&136:
   p=b[t]
   if p:
    if p==c*(3 if i<4 else 4)or p==5*c or p==6*c and t==s+d:return 1
    break
   t+=d
 return 0
def M(q,m):
 b,s,r,e=q;a,z,pr=m;v=b[a];b=b.copy()
 if v==s and z==e:b[z-16*s]=0
 b[a]=0;b[z]=s*pr if pr else v
 if v==6*s and B(z-a)==2:
  h=112*(s<0)
  if z>a:b[h+5]=b[h+7];b[h+7]=0
  else:b[h+3]=b[h];b[h]=0
 return b,-s,r&CR.get(a,15)&CR.get(z,15),a+16*s if v==s and z-a==32*s else-1
def P(o,a,z):o.extend((a,z,t)for t in((2,3,4,5)if z>>4 in(0,7)else(0,)))
def G(q):
 b,s,r,e=q;o=[];k=b.index(6*s);h=112*(s<0)
 for a,p in I(b):
  if p*s<=0:continue
  n=B(p)
  if n==1:
   d=16*s;z=a+d
   if not z&136 and not b[z]:
    P(o,a,z)
    if a>>4==6-5*(s>0)and not b[z+d]:o.append((a,z+d,0))
   for dd in(d-1,d+1):
    z=a+dd
    if not z&136 and(b[z]*s<0 and b[z]!=-6*s or z==e):P(o,a,z)
  else:
   for d in LD[n-2]:
    z=a+d
    while not z&136 and b[z]*s<=0 and b[z]!=-6*s:
     o.append((a,z,0))
     if b[z]or n in(2,6):break
     z+=d
   if n==6 and a==h+4 and not A(b,a,-s):
    for d in(1,-1):
     z=h+4+2*d;t=z-d
     if r&(1<<(1-s))*(1+(d<0))and b[h+7*(d>0)]*s==4 and not any(b[h+i]for i in((5,6)if d>0 else(1,2,3)))and not A(b,t,-s):o.append((a,z,0))
 return[m for m in o if not A(M(q,m)[0],m[1]if b[m[0]]==6*s else k,-s)]
def H(q):
 b,s,r,e=q;h=Z[1543+r+16*(s<0)]
 for i,p in I(b):
  if p:h^=Z[(B(p)-1)*256+(p<0)*128+i]
 return h
def E(q):
 b,s=q[0],q[1];sc=0
 for i,p in I(b):
  if p:
   n=B(p);u=i&7;sc+=p//B(p)*(V[n]+(ord(PT[(n-1)*32+(u if u<4 else 7-u)+4*((i>>4)^(p>0)*7)])-75)*5)
 return sc*s
def OR(q,m):
 b,s,r,e=q;a,z,pr=m;v=b[z]
 if m==tm:return 1<<26
 if v or z==e:return 1000000+V[B(v)or 1]*16-B(b[a])+pr*30000
 return 950000+pr if pr else 0
def S(q,d,a,b,ply):
 global n,tm
 n+=1
 if n>BUD:raise X
 bd,s,r,e=q;chk=A(bd,bd.index(6*s),-s);best=-INF;oa=a
 if d<=0:
  if not chk:
   best=E(q)
   if best>=b:return best
   a=max(a,best)
  ms=G(q)if chk else[m for m in G(q)if bd[m[1]]or m[2]or m[1]==e]
 else:
  if chk:d+=1
  h=H(q);t=tt.get(h);tm=t[3]if t else 0
  if t and ply and t[0]>=d:
   f,v=t[1],t[2]
   if not f or(f>0 and v>=b)or(f<0 and v<=a):return v
  ms=G(q)
 if not ms:return-MATE+ply if chk else best if d<=0 else 0
 ms.sort(key=lambda m:OR(q,m),reverse=True);bm=ms[0]
 for i,m in I(ms):
  v=-S(M(q,m),d-1,-(a+1)if i else-b,-a,ply+1)
  if i and a<v<b:v=-S(M(q,m),d-1,-b,-a,ply+1)
  if v>best:best=v;bm=m
  if v>a:
   a=v
   if a>=b:break
 if d>0:tt[h]=(d,(best>=b)-(best<=oa),best,bm)
 return best
def choose(f,ls):
 global n,tt,tm
 q=F(f);ms=G(q)
 if not ms:return ls[0]
 n=0;tt={};bm=ms[0];h=H(q)
 for d in range(1,80):
  try:S(q,d,-INF,INF,0);bm=tt.get(h,(0,0,0,bm))[3]
  except X:break
 a,z,pr=bm;u=chr(97+(a&7))+chr(49+(a>>4))+chr(97+(z&7))+chr(49+(z>>4))+(' pnbrq'[pr]if pr else'')
 return u if u in ls else ls[0]
post=lambda p,d:json.load(U.urlopen(U.Request(os.environ['MAKEFUN_ENV_URL']+p,json.dumps(d).encode())))
if __name__=='__main__':
 while(g:=post('/game',{}))['status']!='done':
  while g['status']=='your_move':
   try:m=choose(g['fen'],g['legal_moves'])
   except:m=g['legal_moves'][0]
   g=post('/move',{'game':g['game'],'move':m})
