V='game';T='status';S=OverflowError;M=range;G=enumerate;A=abs;import json as H,os,random as W;from urllib.request import Request as X,urlopen as Y;NODE_BUDGET=40000;C=0,100,320,330,500,900,0;Q=14,18,31,33,-14,-16,-31,-33;N=16,-16,1,-1;O=15,17,-15,-17;Z=W.Random(20240827);F=[Z.getrandbits(64)for A in M(1568)]
def parse(f):
	A=f.split();D=[0]*128
	for(E,F)in G(A[0].split('/')):
		C=0
		for B in F:
			if B.isdigit():C+=int(B)
			else:D[(7-E)*16+C]=' pnbrqk'.index(B.lower())*(1 if B.isupper()else-1);C+=1
	return D,(A[1]=='w')*2-1,sum((C in A[2])<<B for(B,C)in G('KQkq')),(int(A[3][1])-1)*16+ord(A[3][0])-97 if A[3]!='-'else-1
def K(b,s,by):
	for C in(-15,-17)if by==1 else(15,17):
		B=s+C
		if not B&136 and b[B]==by:return 1
	for C in Q:
		B=s+C
		if not B&136 and b[B]==by*2:return 1
	for(E,C)in G(N+O):
		B=s+C
		while not B&136:
			D=b[B]
			if D:return D*by>0 and(A(D)==5 or A(D)==4 and E<4 or A(D)==3 and E>3 or A(D)==6 and B==s+C)
			B+=C
def make(st,m):
	B,E,G,L=st;D,C,K=m;B=B.copy();F=B[D];B[D]=0
	if A(F)==1 and C==L and not B[C]:B[C-16*E]=0
	B[C]=K*E if K else F
	if A(F)==6 and A(C-D)==2:H=0 if E==1 else 112;I=H+7 if C>D else H;M=I-2 if C>D else H+3;B[M]=B[I];B[I]=0
	if A(F)==6:G&=3 if E==1 else 12
	for J in(D,C):
		if J in(0,7,112,119):G&=~(1<<J//112*2+(J%112==0))
	return B,-E,G,(D+C)//2 if A(F)==1 and A(C-D)==32 else-1
def R(out,a,c):out.extend((a,c,A)for A in(5,4,3,2))if c>>4 in(0,7)else out.append((a,c,0))
def gen(st,caps=False):
	L=caps;C,D,S,V=st;H=[];W=next(A for(A,B)in G(C)if B==D*6)
	for(E,P)in G(C):
		if not P or P*D<0 or E&136:continue
		J=A(P);F=E>>4
		if J==1:
			I=16*D;B=E+I
			if not B&136 and not C[B]and(not L or B>>4 in(0,7)):
				R(H,E,B);T=B+I
				if not L and B>>4 not in(0,7)and F==(1 if D==1 else 6)and not C[T]:H.append((E,T,0))
			for I in(15,17)if D==1 else(-15,-17):
				B=E+I
				if not B&136 and(C[B]*D<0 and A(C[B])!=6 or B==V):R(H,E,B)
		else:
			X=Q if J==2 else N+O if J in(5,6)else N if J==4 else O
			for I in X:
				B=E+I
				while not B&136:
					if C[B]*D>0 or A(C[B])==6:break
					if not L or C[B]:H.append((E,B,0))
					if C[B]or J in(2,6):break
					B+=I
		if J==6 and not L and not K(C,E,-D):
			F=0 if D==1 else 112
			if S&(1 if D==1 else 4)and not C[F+5]and not C[F+6]and not K(C,F+5,-D):H.append((E,F+6,0))
			if S&(2 if D==1 else 8)and not C[F+1]and not C[F+2]and not C[F+3]and not K(C,F+3,-D):H.append((E,F+2,0))
	U=[]
	for M in H:
		Y=make(st,M);Z=M[1]if A(C[M[0]])==6 else W
		if not K(Y[0],Z,-D):U.append(M)
	return U
def c(st):B,D=st[:2];return sum(B//A(B)*(C[A(B)]+3-A(3-i%8))for i,B in G(B)if B)*D
def I(m):A,B,C=m;return'%c%c%c%c%s'%(97+(A&7),49+(A>>4),97+(B&7),49+(B>>4),'pnbrq'[C-1:C])
def e(st):
	H,I,J,C=st;D=F[1536+J+(I<0)*16]^(F[C*12]if C>=0 else 0)
	for(E,B)in G(H):
		if B and not E&136:D^=F[E*12+(A(B)-1)*2+(B<0)]
	return D
def f(ms,st,tt):
	B=st[0]
	def D(m):D,E,G=m;F=B[E]or-(A(B[D])==1 and E==st[3]);return m==tt,bool(F),C[A(F)]*10-C[A(B[D])],G
	return sorted(ms,key=D,reverse=True)
def P(st,d,a,b,ply):
	global D;D+=1
	if D>NODE_BUDGET:raise S
	N,O=st[:2];F=K(N,next(A for(A,B)in G(N)if B==O*6),-O);A=d<1;I=-32000 if F else c(st)if A else 0
	if A and not F:
		if I>=b:return b
		a=max(a,I)
	U=a;Q=e(st)if not A else 0;V,J,L,W=E.get(Q,[0]*4)if not A else(-1,0,0,0)
	if V>=d and(not L or L>0 and J>=b or L<0 and J<=a):return J
	M=gen(st,A and not F)
	if not M:return-30000+ply if F else I
	C=-3e4;R=M[0];X=d-1+A
	for T in f(M,st,W):
		H=-P(make(st,T),X,-b,-a,ply+1)
		if H>C:C=H;R=T
		if H>a:a=H
		if a>=b:break
	if not A:E[Q]=d,C,(C>=b)-(C<=U),R
	return C
def choose(fen,legal):
	B=legal;global D,E;C=parse(fen);A=[A for A in gen(C)if I(A)in B]
	if not A:return B[0]
	F=A[0];D=0;E={}
	for G in M(1,64):
		try:F=max(A,key=lambda m:-P(make(C,m),G-1,-32000,32000,1))
		except S:break
	return I(F)
def J(p,x):return H.load(Y(X(a+p,H.dumps(x).encode(),{'content-type':'application/json'})))
if __name__=='__main__':
	a=os.getenv('MAKEFUN_ENV_URL')
	while(B:=J('/game',{}))[T]!='done':
		while B[T]=='your_move':
			try:L=choose(B['fen'],B[U])
			except:L=B['legal_moves'][0]
			B=J('/move',{V:B[V],'move':L})