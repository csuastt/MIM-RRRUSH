function main()                   %无返回值函数的定义方法
    clc;clear all;close all;
    
    tic;                              %程序运行计时
    E0=0.001;                        %允许误差
    MaxNum=100;                    %粒子最大迭代次数
    narvs=1;                         %目标函数的自变量个数，也就是粒子在空间中的坐标维度，有几个维度
    particlesize=30;                    %粒子群规模
    c1=2;                            %每个粒子的个体学习因子，也称为加速常数，即这个粒子的历史最优值（位置）对位置更新的影响程度
    c2=2;                            %每个粒子的社会学习因子，也称为加速常数，即全局的最优值（位置）对位置更新的影响程度
    w=0.6;                           %惯性因子，即粒子的速度对位置更新的权重，c1的权重为 c1/(c1+c2)*(1-w)
    vmax=0.8;                        %粒子的最大飞翔速度，即每次更新时的速度v应该处于如下范围-vmax <= v <= vmax
    x=-5+10*rand(particlesize,narvs);     %粒子所在的位置，每个粒子有narvs个坐标
    v=2*rand(particlesize,narvs);         %粒子的飞翔速度，每个坐标都有自己的速度
    
    %用inline定义适应度函数以便将子函数文件与主程序文件放在一起，inline定义的函数也可以输入向量
    %目标函数是：y=1+(2.1*(1-x+2*x.^2).*exp(-x.^2/2))
    %inline命令定义适应度函数如下：
    fitness=inline('1/(1+(2.1*(1-x+2*x.^2).*exp(-x.^2/2)))','x'); %！！此处替换为题目对应的适应度函数
    %fitness=inline('x(1) + x(2)', 'x'); % 使用向量作为输入直接定义目标函数
    %inline定义的适应度函数会使程序运行速度大大降低
    
    if ~exist('fitness', 'var')
        disp('Please define inline function: fitness()');
    end
    
    % 计算出每个位置的适应程度fitness：将这个位置代入目标函数，其值越低越符合要求
    for i=1:particlesize % 遍历每个粒子
        % f(i) = fitness(x(i,:));
        for j=1:narvs
            f(i)=fitness(x(i,j));
        end
    end
    
    personalbest_x=x; % 每个粒子自己的历史最优值（位置）
    personalbest_faval=f; % 每个粒子自己的历史最小的fitness
    [globalbest_faval i]=min(personalbest_faval); % 全局的最优的位置
    globalbest_x=personalbest_x(i,:);
    
    % 上面都是准备工作，现在开始迭代，k是当前迭代的轮次
    k=1;
    while k<=MaxNum % 当前迭代的轮次小于最大的轮次就仍然继续迭代
        for i=1:particlesize
    
            % f(i) = fitness(x(i,:));
            % 下面应该是输入一个向量，然后得到一个fitness值
            for j=1:narvs
                f(i)=fitness(x(i,j));
            end
            % 上面应该是输入一个向量，然后得到一个fitness值
    
            if f(i)<personalbest_faval(i) %判断当前位置是否是历史上最佳位置
                personalbest_faval(i)=f(i);
                personalbest_x(i,:)=x(i,:);
            end
        end
    
        % 更新全局的最优位置以及最低的fitness
        [globalbest_faval i]=min(personalbest_faval);
        globalbest_x=personalbest_x(i,:);
    
        % 对每个粒子来说根据自己的历史最优位置以及全局历史最优位置更新速度（前进方向）
        for i=1:particlesize
            v(i,:)=w*v(i,:)+c1*rand*(personalbest_x(i,:)-x(i,:))...
                +c2*rand*(globalbest_x-x(i,:));
    
            for j=1:narvs    %判断粒子的飞翔速度是否超过了最大飞翔速度
                if v(i,j)>vmax;
                    v(i,j)=vmax;
                elseif v(i,j)<-vmax;
                    v(i,j)=-vmax;
                end
            end
    
            x(i,:)=x(i,:)+v(i,:); % 更新粒子群里每个个体的最新位置
        end
    
        % 如果误差已经足够小，即fitness已经下降到一定的程度，那么就停止更新，使用历史最优位置作为最终结果
        if abs(globalbest_faval)<E0,break,end
        k=k+1;
    end
    
    Value1=1/globalbest_faval-1;
    Value1=num2str(Value1);
    % strcat指令可以实现字符的组合输出
    disp(strcat('the maximum value','=',Value1));
    
    %输出最大值所在的坐标位置（原始的代码中只定义了一个坐标位置）,这里之所以说是最大值，是因为fitness函数定义的是反过来的。
    Value2=globalbest_x; Value2=num2str(Value2);
    disp(strcat('the corresponding coordinate','=',Value2));
    
    x=-5:0.01:5;
    y=2.1*(1-x+2*x.^2).*exp(-x.^2/2);
    plot(x,y,'m-','linewidth',3);
    hold on;
    plot(globalbest_x,1/globalbest_faval-1,'kp','linewidth',4);
    legend('目标函数','搜索到的最大值');xlabel('x');ylabel('y');grid on;toc;