import os,json,random
import urllib.request as U
NODE_BUDGET=18000
I=enumerate;B=abs;J=range;C=chr
V=(0,100,320,330,500,900,0);D=(16,-16,1,-1,15,17,-15,-17);N=(33,31,18,14,-14,-18,-31,-33)
L=(N,D[4:],D[:4],D,D,D)
R={0:2,7:1,112:8,119:4};random.seed(74123);Z=[random.getrandbits(64)for _ in J(1561)]
class X(Exception):pass
def F(f):
 a=f.split();b=[0]*128
 for y,row in I(a[0].split('/')):
  x=0
  for c in row:
   if c<'9':x+=int(c);continue
   b[(7-y)*16+x]=(' pnbrqk'.find(c.lower()))*(1-2*(c>'Z'));x+=1
 return b,2*(a[1]>'b')-1,sum((c in a[2])<<i for i,c in I('KQkq')),-1 if a[3]=='-'else(int(a[3][1])-1)*16+ord(a[3][0])-97
def A(b,s,c):
 for i,d in I((-15*c,-17*c)+N):
  t=s+d
  if not t&136 and b[t]==c*(1+(i>1)):return 1
 for i,d in I(D):
  t=s+d
  while not t&136:
   p=b[t]
   if p:
    if p*c in(5,4-i//4,6*(t==s+d)):return 1
    break
   t+=d
def M(q,m):
 b,s,r,e=q;a,z,p=m;v=b[a];b=b.copy()
 if v==s and z==e:b[z-16*s]=0
 b[a]=0;b[z]=s*p if p else v
 if v==6*s and B(z-a)==2:
  h=112*(s<0);x=h+7*(z>a);y=x-2 if z>a else h+3;b[y]=b[x];b[x]=0
 if v==6*s:r&=3<<(1+s)
 r&=~R.get(a,0)&~R.get(z,0)
 return b,-s,r,a+16*s if v==s and z-a==32*s else-1
def P(o,a,z):o.extend((a,z,t)for t in(J(2,6)if z>>4 in(0,7)else(0,)))
def G(q,c=0):
 b,s,r,e=q;o=[];k=b.index(6*s);h=112*(s<0);x=1<<(1-s)
 for a,p in I(b):
  if p*s<=0:continue
  n=B(p)
  if n==1:
   d=16*s;z=a+d
   if not z&136 and not b[z]and(not c or z>>4 in(0,7)):
    P(o,a,z);t=z+d
    if not c and a>>4==6-5*(s>0)and not b[t]:o.append((a,t,0))
   for d in(d-1,d+1):
    z=a+d
    if not z&136 and(b[z]*s<0 and b[z]!=-6*s or z==e):
     P(o,a,z)
  else:
   for d in L[n-2]:
    z=a+d
    while not z&136:
     if b[z]*s>0 or b[z]==-6*s:break
     if not c or b[z]:o.append((a,z,0))
     if b[z]or n in(2,6):break
     z+=d
  if n==6 and a==h+4 and not c and not A(b,a,-s):
   for d in(1,-1):
    z=h+4+2*d;t=z-d
    if r&x*(1+(d<0))and b[h+7*(d>0)]*s==4 and not any(b[h+i]for i in((5,6)if d>0 else(1,2,3)))and not A(b,t,-s):o.append((a,z,0))
 return [m for m in o if not A(M(q,m)[0],m[1]if b[m[0]]==6*s else k,-s)]
def T(m):
 a,z,p=m;return C(97+(a&7))+C(49+(a>>4))+C(97+(z&7))+C(49+(z>>4))+(' pnbrq'[p]if p else'')
def H(q):
 b,s,r,e=q;h=Z[1536]*(s<0)^Z[1537+r]^Z[1553+(e&7)]*(e>=0)
 for i,p in I(b):
  if p:h^=Z[(B(p)-1)*256+(p<0)*128+i]
 return h
def Vv(q):
 b,s=q[:2];m=e=p=0
 for v in b:
  if not v:continue
  n=B(v);t=v//n;m+=t*V[n];e+=t*(V[n]+10*(n==1));p+=(0,0,1,1,2,4,0)[n]
 p=min(24,p)
 return((m*p+e*(24-p))//24)*s
def O(q,m,l):
 b,s,r,e=q;a,z,p=m;v=b[z]or(1 if z==e and B(b[a])==1 else 0)
 return m==tm,p*18000+(12000+V[B(v)]*10-V[B(b[a])]if v else 0),m in K[l]
def Q(q,d,a,b,l):
 global n,tm
 if (n:=n+1)>NODE_BUDGET:raise X
 x,s=q[:2];c=A(x,x.index(6*s),-s)
 if d<=0:
  if not c:
   a=max(a,Vv(q))
   if a>=b:return b
  ms=G(q,not c)
  if c and not ms:return-30000
  ms.sort(key=lambda m:(x[m[1]]or m[1]==q[3],m[2],V[B(x[m[1]])or 1]),reverse=True)
  for m in ms:
   a=max(a,-Q(M(q,m),0,-b,-a,l+1))
   if a>=b:return b
  return a
 o=a;h=H(q);t=tt.get(h);tm=t[3]if t else 0
 if t and t[0]>=d and(not t[1]or t[1]*(t[2]-(b if t[1]>0 else a))>=0):return t[2]
 ms=G(q)
 if not ms:return-30000*(c or 0)
 ms.sort(key=lambda m:O(q,m,l),reverse=True);v=-32000;bm=ms[0]
 for m in ms:
  t=not(x[m[1]]or m[2]or m[1]==q[3]);u=M(q,m)
  w=-Q(u,d-1,-b,-a,l+1)
  if w>v:v=w;bm=m
  if w>a:a=w
  if a>=b:K[l]=[m,K[l][0]] if t else K[l];break
 tt[h]=(d,(v>=b)-(v<=o),v,bm);return v
def choose(f,ls):
 global n,tt,K,tm
 q=F(f);r=G(q)
 if not r:return ls[0]
 n=0;tt={};K=[[0,0]]*80;tm=0;m=r[0];h=H(q)
 for d in J(1,40):
  try:Q(q,d,-32000,32000,0);m=tt[h][3]
  except X:break
 m=T(m);return m if m in ls else ls[0]
post=lambda p,d:json.load(U.urlopen(U.Request(os.getenv('MAKEFUN_ENV_URL')+p,json.dumps(d).encode(),{'content-type':'application/json'})))
if __name__=='__main__':
 while(g:=post('/game',{}))['status']!='done':
  while g['status']=='your_move':
   try:m=choose(g['fen'],g['legal_moves'])
   except Exception:m=g['legal_moves'][0]
   g=post('/move',{'game':g['game'],'move':m})
