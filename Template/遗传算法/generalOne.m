tic
clear,clc;

counts=100;
w=50;%种群大小
dai=300;%代数
L=counts+2;%中间加起点、终点

%对应x y坐标
load sj.txt
x=sj(:,1:2:8);
x=x(:);
y=sj(:,2:2:8);
y=y(:);
sj=[x y];
d1=[70,40];
sj0=[d1;sj;d1];
%距离矩阵
sj=sj0;
d=zeros(counts+2);
for i=1:(counts+1)
    for j=i+1:(counts+2)
        temp=sqrt((sj(i,1)-sj(j,1))^2+(sj(i,2)-sj(j,2))^2);
        d(i,j)=temp;
    end
end
d=d+d';

%改良圈
for k=1:w
    c=randperm(counts);
    c1=[1,c+1,counts+2];
    flag=1;
    while flag>0
        flag=0;
        for m=1:L-3
            for n=m+2:L-1
                if d(c1(m),c1(n))+d(c1(m+1),c1(n+1))<d(c1(m),c1(m+1))+d(c1(n),c1(n+1))
                    flag=1;
                    c1(m+1:n)=c1(n:-1:m+1);
                end
            end
        end
    end
    J(k,c1)=1:(counts+2);
end
J=J/(counts+2);
J(:,1)=0;
J(:,(counts+2))=1;
rand('state',sum(clock));
%遗传算法
A=J;
for k=1:dai
    B=A;
    c=randperm(w);
    %交配
    for i=1:2:w
        F=2+floor(100*rand(1));
        temp=B(c(i),F:(counts+2));
        B(c(i),F:(counts+2))=B(c(i+1),F:(counts+2));
        B(c(i+1),F:(counts+2))=temp;
    end
    %变异
    by=find(rand(1,w)<0.1);
    if length(by)==0
        by=floor(w*rand(1))+1;
    end
    C=A(by,:);
    L3=length(by);
    for j=1:L3
        bw=2+floor(counts*rand(1,3));
        bw=sort(bw);
        C(j,:)=C(j,[1:bw(1)-1,bw(2)+1:bw(3),bw(1):bw(2),bw(3)+1:(counts+2)]);
    end
    G=[A;B;C];
    TL=size(G,1);
    %在父代和子代中选择优良品种作为新的父代
    [dd,IX]=sort(G,2);
    temp(1:TL)=0;
    for j=1:TL
        for i=1:(counts+1)
            temp(j)=temp(j)+d(IX(j,i),IX(j,i+1));
        end
    end
    [DZ,IZ]=sort(temp);
    A=G(IZ(1:w),:);
end
path=IX(IZ(1),:)
long=DZ(1)
toc
xx=sj0(path,1);
yy=sj0(path,2);
plot(xx,yy,'-o');