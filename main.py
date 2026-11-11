import json,os,urllib.request
NODE_BUDGET=30000
V=(0,100,320,330,500,900,0);N=(14,18,31,33,-14,-18,-31,-33);O=(16,-16,1,-1);D=(15,17,-15,-17)
n=0
def parse(f):
 b=[0]*128
 for r,row in enumerate(f.split()[0].split('/')):
  x=0
  for c in row:
   if c.isdigit():x+=int(c)
   else:b[(7-r)*16+x]=(' pnbrqk'.index(c.lower()))*(1 if c.isupper()else-1);x+=1
 t=f.split();return b,1 if t[1]=='w'else-1,sum(1<<i for i,c in enumerate('KQkq')if c in t[2]),(int(t[3][1])-1)*16+ord(t[3][0])-97 if t[3]!='-'else-1
def att(b,s,by):
 for d in (-15,-17)if by==1 else(15,17):
  q=s+d
  if not q&136 and b[q]==by:return 1
 for d in N:
  q=s+d
  if not q&136 and b[q]==by*2:return 1
 if any(not((s+d)&136)and b[s+d]==by*6 for d in O+D):return 1
 for i,d in enumerate(O+D):
  q=s+d
  while not q&136:
   p=b[q]
   if p:return p*by>0 and(abs(p)==5 or abs(p)==4 and i<4 or abs(p)==3 and i>3)
   q+=d
def make(st,m):
 b,s,cr,ep=st;a,c,p=m;b=b.copy();x=b[a];cap=b[c];b[a]=0
 if abs(x)==1 and c==ep and not cap:b[c-16*s]=0
 b[c]=p*s if p else x
 if abs(x)==6 and abs(c-a)==2:
  r=0 if s==1 else 112;h=r+7 if c>a else r;t=h-2 if c>a else r+3;b[t]=b[h];b[h]=0
 if abs(x)==6:cr&=3 if s==1 else 12
 for q in(a,c):
  if q in(0,7,112,119):cr&=~(1<<((q//112)*2+(q%112==7)))
 return b,-s,cr,(a+c)//2 if abs(x)==1 and abs(c-a)==32 else-1
def gen(st,caps=False):
 b,s,cr,ep=st;out=[];king=next(i for i,x in enumerate(b)if x==s*6)
 for a,x in enumerate(b):
  if not x or x*s<0 or a&136:continue
  p=abs(x);r=a>>4
  if p==1:
   d=16*s;c=a+d
   if not c&136 and not b[c]and(not caps or c>>4 in(0,7)):
    out.extend((a,c,q)for q in(5,4,3,2))if c>>4 in(0,7)else out.append((a,c,0))
    z=c+d
    if not caps and c>>4 not in(0,7)and r==(1 if s==1 else 6)and not b[z]:out.append((a,z,0))
   for d in(15,17)if s==1 else(-15,-17):
    c=a+d
    if not c&136 and((b[c]*s<0 and abs(b[c])!=6)or c==ep):
     out.extend((a,c,q)for q in(5,4,3,2))if c>>4 in(0,7)else out.append((a,c,0))
  else:
   ds=N if p==2 else O+D if p in(5,6)else O if p==4 else D
   for d in ds:
    c=a+d
    while not c&136:
     if b[c]*s>0 or abs(b[c])==6:break
     if not caps or b[c]:out.append((a,c,0))
     if b[c]or p in(2,6):break
     c+=d
  if p==6 and not caps and not att(b,a,-s):
   r=0 if s==1 else 112
   if cr&(1 if s==1 else 4)and not b[r+5]and not b[r+6]and not att(b,r+5,-s):out.append((a,r+6,0))
   if cr&(2 if s==1 else 8)and not b[r+1]and not b[r+2]and not b[r+3]and not att(b,r+3,-s):out.append((a,r+2,0))
 legal=[]
 for m in out:
  n=make(st,m);k=m[1] if abs(b[m[0]])==6 else king
  if not att(n[0],k,-s):legal.append(m)
 return legal
def ev(st):
 b,s=st[:2]
 return sum((1 if p>0 else-1)*(V[abs(p)]+3-abs(3-(a&7)))for a,p in enumerate(b)if p)*s
def uci(m):
 a,b,p=m
 return chr(97+(a&7))+str(1+(a>>4))+chr(97+(b&7))+str(1+(b>>4))+(' nbrq'[p]if p else'')
def order(ms,st):
 b=st[0]
 return sorted(ms,key=lambda m:(bool(b[m[1]]),V[abs(b[m[1]])]*10-V[abs(b[m[0]])]if b[m[1]]else 0,m[2]),reverse=True)
def search(st,d,a,b,ply=0):
 global n
 n+=1
 if n>NODE_BUDGET:raise OverflowError
 if d<1:return ev(st)
 ms=gen(st)
 if not ms:
  k=next(i for i,x in enumerate(st[0])if x==st[1]*6)
  return-30000+ply if att(st[0],k,-st[1])else 0
 best=-32000
 for m in order(ms,st):
  v=-search(make(st,m),d-1,-b,-a,ply+1)
  if v>best:best=v
  if v>a:a=v
  if a>=b:break
 return best
def choose(fen,legal):
 global n
 st=parse(fen);ms=[m for m in gen(st)if uci(m)in legal]
 if not ms:return legal[0]
 best=ms[0];n=0
 for d in range(1,64):
  try:
   vbest=-32000;pick=best
   for m in ms:
    v=-search(make(st,m),d-1,-32000,32000,1)
    if v>vbest:vbest=v;pick=m
   best=pick
  except OverflowError:break
 return uci(best)
def post(u,p,x):
 q=urllib.request.Request(u+p,json.dumps(x).encode(),{'content-type':'application/json'})
 return json.load(urllib.request.urlopen(q))
if __name__=='__main__':
 u=os.environ['MAKEFUN_ENV_URL']
 while(g:=post(u,'/game',{}))['status']!='done':
  while g['status']=='your_move':
   try:m=choose(g['fen'],g['legal_moves'])
   except Exception:m=g['legal_moves'][0]
   g=post(u,'/move',{'game':g['game'],'move':m})
