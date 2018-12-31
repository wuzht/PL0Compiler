{PL0编译程序注释}

Program  PL0 (input, output);
{带有代码生成的PL0编译程序}

Const 
  norw = 13; {保留字的个数}
  txmax = 100; {标识符表长度}
  nmax = 14; {数字的最大位数}
  al = 10; {标识符的长度}
  amax = 2047; {最大地址}
  levmax = 3; {程序体嵌套的最大深度}
  cxmax = 200; {代码数组的大小}

Type 
  symbol = (nul, ident, number, plus, minus, times, slash, oddsym,
            eql, neq, lss, leq, gtr, geq, lparen, rparen, comma, semicolon,
            period, becomes, beginsym, endsym, ifsym, thensym,
            whilesym, dosym, callsym, constsym, varsym, procsym,
            readsym, writesym);
  alfa = packed array [1..al] Of char;
  Object2 = (constant, variable, Procedure2);
  symset = set Of symbol;
  fct = (lit, opr, lod, sto, cal, int, jmp, jpc, red, wrt); {functions}
  instruction = packed Record
    f : fct;  {功能码}
    l : 0..levmax; {相对层数}
    a : 0..amax; {相对地址}
End;

{LIT 0,a : 取常数a
OPR 0,a : 执行运算a
LOD l,a : 取层差为l的层﹑相对地址为a的变量
STO l,a : 存到层差为l的层﹑相对地址为a的变量
CAL l,a : 调用层差为l的过程
INT 0,a : t寄存器增加a
JMP 0,a : 转移到指令地址a处
JPC 0,a : 条件转移到指令地址a处 }

Var 
  ch : char; {最近读到的字符}
  sym : symbol; {最近读到的符号}
  id : alfa; {最近读到的标识符}
  num : integer; {最近读到的数}
  cc : integer; {当前行的字符计数}
  ll : integer; {当前行的长度}
  kk, err : integer;
  cx : integer; {代码数组的当前下标}
  line : array [1..81] Of char; {当前行}
  a : alfa; {当前标识符的字符串}
  code : array [0..cxmax] Of instruction; {中间代码数组}
  word : array [1..norw] Of alfa; {存放保留字的字符串}
  wsym : array [1..norw] Of symbol; {存放保留字的记号}
  ssym : array [char] Of symbol; {存放算符和标点符号的记号}
  mnemonic : array [fct] Of packed array [1..5] Of char;
  {中间代码算符的字符串}
  declbegsys, statbegsys, facbegsys : symset;
  table : array [0..txmax] Of {符号表}
    Record
      name : alfa;
      Case kind : Object2 Of constant : (val : integer);
      variable, Procedure2 : (level, adr : integer)
    End;
  fin, fout : text; {fin, fout是文本文件}
  srcfile, dstfile : string;


Procedure error (n : integer);
Begin
  writeln('****', ' ' : cc-1, '^', n : 2);
  err := err + 1
  {cc为当前行已读的字符数, n为错误号}{错误数err加1}
End {error};


Procedure getsym;
  Var  i, j, k : integer;
  Procedure  getch; {取下一字符}
  Begin If cc = ll Then {如果cc指向行末}
    Begin If eof(fin) Then {如果已到文件尾}
        Begin
          write('Program INCOMPLETE');
          writeln(fout, 'Program INCOMPLETE');
          close(fin);
          close(fout);
          // goto 99
          exit(); {相当于goto 99}
        End;
      {读新的一行}
      ll := 0;
      cc := 0;
      write(cx : 5, ' '); {cx : 5位数}
      write(fout, cx : 5, ' ');
      While not eoln(fin) Do {如果不是行末}
        Begin
          ll := ll + 1;
          read(fin, ch);
          write(ch);
          write(fout, ch);
          line[ll] := ch  {一次读一行入line}
        End;
      writeln;
      writeln(fout);
      readln(fin);
      ll := ll + 1;
      // read(line[ll])  {line[ll]中是行末符}
      line[ll] := ' '
    End;
    cc := cc + 1;
    ch := line[cc]  {ch取line中下一个字符}
  End {getch};

Begin {getsym}
  while ch = ' ' do 
    getch; {跳过无用空白}
  If ch In ['a'..'z'] Then
    Begin {标识符或保留字}
      k := 0;
      Repeat {处理字母开头的字母﹑数字串}
        If k < al Then
        Begin
          k := k + 1;
          a[k] := ch
        End;
        getch
      Until not (ch In ['a'..'z', '0'..'9']);
      If k >= kk Then 
        kk := k
      Else Repeat
        a[kk] := ' ';
        kk := kk-1  {如果标识符长度不是最大长度, 后面补空白}
      until kk = k;                
      id := a;
      i := 1;
      j := norw;
      {id中存放当前标识符或保留字的字符串}
      Repeat
        k := (i+j) Div 2; {用二分查找法在保留字表中找当前的标识符id}
        if id <= word[k] then j := k-1;  
        If id >= word[k] Then i := k+1
      Until i > j;
      If i-1 > j Then sym := wsym[k] Else sym := ident
      {如果找到, 当前记号sym为保留字, 否则sym为标识符}
    End
      
  Else If ch In ['0'..'9'] Then
    Begin {数字}
      k := 0; num := 0; sym := number; {当前记号sym为数字}
      Repeat {计算数字串的值}
        num := 10*num + (ord(ch)-ord('0'));
        {ord(ch)和ord(0)是ch和0在ASCII码中的序号}
        k := k + 1;
        getch;
      Until not (ch In ['0'..'9']); {直到输入的不是数字}
      If k > nmax Then error(30)
      {当前数字串的长度超过上界,则报告错误}
    End
      
  Else If ch = ':' Then {处理赋值号}
    Begin
      getch;
      If ch = '=' Then
        Begin
          sym := becomes;
          getch
        End
      Else 
        sym := nul;
    End

  Else {处理其它算符或标点符号}
    Begin
      sym := ssym[ch];
      getch
    End
End {getsym};


Procedure  gen(x : fct; y, z : integer);
Begin
  If cx > cxmax Then {如果当前指令序号>代码的最大长度}
    Begin
      write('Program TOO LONG');
      writeln(fout);
      close(fin);
      // goto 99
      exit();
    End;
  With code[cx] Do {在代码数组cx位置生成一条新代码}
    Begin
      f := x; {功能码}
      l := y; {层号}
      a := z {地址}
    End;
  cx := cx + 1 {指令序号加1}
End {gen};


Procedure  test(s1, s2 : symset; n : integer);
Begin
  If not (sym In s1) Then
  {如果当前记号不属于集合S1,则报告错误n}
    Begin
      error(n);
      s1 := s1 + s2;
      While not (sym In s1) Do
        getsym
        {跳过一些记号, 直到当前记号属于S1∪S2}
    End
End {test};


Procedure  block(lev, tx : integer; fsys : symset);
  Var dx : integer; {本过程数据空间分配下标}
      tx0 : integer; {本过程标识表起始下标}
      cx0 : integer; {本过程代码起始下标}

  Procedure  enter(k : Object2);
  Begin {把object填入符号表中}
    tx := tx +1; {符号表指针加1}
    With table[tx] Do{在符号表中增加新的一个条目}
    Begin
      name := id; {当前标识符的名字}
      kind := k; {当前标识符的种类}
      Case k Of 
        constant :
          Begin {当前标识符是常数名}
            If num > amax Then {当前常数值大于上界,则出错}
              Begin error(30); num := 0 End;
            val := num
          End;
        variable :
          Begin {当前标识符是变量名}
            level := lev; {定义该变量的过程的嵌套层数}
            adr := dx; {变量地址为当前过程数据空间栈顶}
            dx := dx +1; {栈顶指针加1}
          End;
        Procedure2 : level := lev {本过程的嵌套层数}
      End
    End
  End {enter};

  Function  position(id : alfa) : integer; {返回id在符号表的入口}
    Var  i : integer;
  Begin {在标识符表中查标识符id}
    table[0].name := id; {在符号表栈的最下方预填标识符id}
    i := tx; {符号表栈顶指针}
    While table[i].name <> id Do i := i-1;
    {从符号表栈顶往下查标识符id}
    position := i {若查到,i为id的入口,否则i=0 }
  End {position};

  Procedure constdeclaration;
  Begin
    If sym = ident Then {当前记号是常数名}
      Begin
        getsym;
        If sym In [eql, becomes] Then {当前记号是等号或赋值号}
          Begin
            If sym = becomes Then error(1);
              {如果当前记号是赋值号,则出错}
            getsym;
            If sym = number Then {等号后面是常数}
              Begin
                enter(constant); {将常数名加入符号表}
                getsym
              End
            Else error(2) {等号后面不是常数出错}
          End
        Else error(3) {标识符后不是等号或赋值号出错}
      End
    Else error(4) {常数说明中没有常数名标识符}
  End {constdeclaration};

  Procedure  vardeclaration;
  Begin
    If sym = ident Then {如果当前记号是标识符}
      Begin
        enter(variable); {将该变量名加入符号表的下一条目}
        getsym
      End
    Else error(4) {如果变量说明未出现标识符,则出错}
  End {vardeclaration};

  Procedure  listcode;
    Var  i : integer;
  Begin  {列出本程序体生成的代码}
    For i := cx0 To cx-1 Do
    {cx0: 本过程第一个代码的序号, cx-1: 本过程最后一个代码的序号}
      With code[i] Do {打印第i条代码}
        writeln(fout, i:3, mnemonic[f] : 5, l : 3, a : 5)
    {i: 代码序号; 
    mnemonic[f]: 功能码的字符串;
    l: 相对层号(层差);
    a: 相对地址或运算号码}
  End {listcode};

  Procedure  statement(fsys : symset);
    Var  i, cx1, cx2 : integer;
    Procedure  expression(fsys : symset);
      Var  addop : symbol;
      Procedure  term(fsys : symset);
        Var  mulop : symbol;
        Procedure  factor(fsys : symset);
          Var i : integer;
          Begin
            test(facbegsys, fsys, 24);
            {测试当前的记号是否因子的开始符号, 
            否则出错, 跳过一些记号}
            While sym In facbegsys Do
            {如果当前的记号是否因子的开始符号}
              Begin
                If sym = ident Then {当前记号是标识符}
                  Begin
                    i := position(id); {查符号表,返回id的入口}
                    If i = 0 Then error(11)
                    Else
                    {若在符号表中查不到id, 则出错, 否则,做以下工作}
                    With table[i] Do
                      Case kind Of 
                        constant : gen(lit, 0, val);
                        {若id是常数, 生成指令,将常数val取到栈顶}
                        variable : gen(lod, lev-level, adr);
                        {若id是变量, 生成指令,将该变量取到栈顶;
                          lev: 当前语句所在过程的层号;
                          level: 定义该变量的过程层号;
                          adr: 变量在其过程的数据空间的相对地址}
                        Procedure2 : error(21)
                        {若id是过程名, 则出错}
                      End;
                    getsym {取下一记号}
                  End

                Else If sym = number Then {当前记号是数字}
                  Begin
                    If num > amax Then {若数值越界,则出错}
                    Begin
                      error(30);
                      num := 0
                    End;
                    gen(lit, 0, num);
                    {生成一条指令, 将常数num取到栈顶}
                    getsym {取下一记号}
                  End

                Else If sym = lparen Then {如果当前记号是左括号}
                  Begin
                    getsym; {取下一记号}
                    expression([rparen]+fsys); {处理表达式}
                    If sym = rparen Then getsym
                    {如果当前记号是右括号, 则取下一记号,否则出错}
                    Else error(22)
                  End;

              test(fsys, [lparen], 23)
              {测试当前记号是否同步, 否则出错, 跳过一些记号}
              End {while}
          End {factor};

        Begin {term}
          factor(fsys+[times, slash]); {处理项中第一个因子}
          While sym In [times, slash] Do
          {当前记号是“乘”或“除”号}
            Begin
              mulop := sym; {运算符存入mulop}
              getsym; {取下一记号}
              factor(fsys+[times, slash]); {处理一个因子}
              If mulop = times Then gen(opr, 0, 4)
              {若mulop是“乘”号,生成一条乘法指令}
              Else gen(opr, 0, 5)
              {否则, mulop是除号, 生成一条除法指令}
            End
        End {term};

      Begin {expression}
        If sym In [plus, minus] Then {若第一个记号是加号或减号}
          Begin
            addop := sym;  {“+”或“-”存入addop}
            getsym;
            term(fsys+[plus, minus]); {处理一个项}
            If addop = minus Then gen(opr, 0, 1)
            {若第一个项前是负号, 生成一条“负运算”指令}
          End
        Else term(fsys+[plus, minus]);
        {第一个记号不是加号或减号, 则处理一个项}
        While sym In [plus, minus] Do {若当前记号是加号或减号}
          Begin
            addop := sym; {当前算符存入addop}
            getsym; {取下一记号}
            term(fsys+[plus, minus]); {处理一个项}
            If addop = plus Then gen(opr, 0, 2)
            {若addop是加号, 生成一条加法指令}
            Else gen(opr, 0, 3)
            {否则, addop是减号, 生成一条减法指令}
          End
      End {expression};

    Procedure  condition(fsys : symset);
      Var  relop : symbol;
      Begin
        If sym = oddsym Then {如果当前记号是“odd”}
          Begin
            getsym;  {取下一记号}
            expression(fsys); {处理算术表达式}
            gen(opr, 0, 6)
            {生成指令,判定表达式的值是否为奇数,
            是,则取“真”;不是, 则取“假”}
          End
        Else {如果当前记号不是“odd”}
          Begin
            expression([eql, neq, lss, gtr, leq, geq] + fsys);
            {处理算术表达式}
            If not (sym In [eql, neq, lss, leq, gtr, geq]) Then
            {如果当前记号不是关系符, 则出错; 否则,做以下工作}
              error(20)
            Else
              Begin
                relop := sym; {关系符存入relop}
                getsym; {取下一记号}
                expression(fsys); {处理关系符右边的算术表达式}
                Case relop Of 
                  eql : gen(opr, 0, 8);
                  {生成指令, 判定两个表达式的值是否相等}
                  neq : gen(opr, 0, 9);
                  {生成指令, 判定两个表达式的值是否不等}
                  lss : gen(opr, 0, 10);
                  {生成指令,判定前一表达式是否小于后一表达式}
                  geq : gen(opr, 0, 11);
                  {生成指令,判定前一表达式是否大于等于后一表达式}
                  gtr : gen(opr, 0, 12);
                  {生成指令,判定前一表达式是否大于后一表达式}
                  leq : gen(opr, 0, 13);
                  {生成指令,判定前一表达式是否小于等于后一表达式}
                End
              End
            End
        End {condition};

    Begin {statement}
      If sym = ident Then {处理赋值语句}
        Begin
          i := position(id);
          {在符号表中查id, 返回id在符号表中的入口}
          If i = 0 Then error(11)
          {若在符号表中查不到id, 则出错, 否则做以下工作}
          Else If table[i].kind <> variable Then
            {若标识符id不是变量, 则出错}
            Begin {对非变量赋值}
              error(12);
              i := 0;
            End;
          getsym; {取下一记号}
          If sym = becomes Then getsym
          Else error(13);
          {若当前是赋值号, 取下一记号, 否则出错}
          expression(fsys); {处理表达式}
          If i <> 0 Then {若赋值号左边的变量id有定义}
            With table[i] Do gen(sto, lev-level, adr)
            {生成一条存数指令, 将栈顶(表达式)的值存入变量id中;
              lev: 当前语句所在过程的层号;
              level: 定义变量id的过程的层号;
              adr: 变量id在其过程的数据空间的相对地址}
        End
          
      Else If sym = callsym Then {处理过程调用语句}
        Begin
          getsym; {取下一记号}
          If sym <> ident Then error(14)
          Else
          {如果下一记号不是标识符(过程名),则出错,
            否则做以下工作}
            Begin
              i := position(id); {查符号表,返回id在表中的位置}
              If i = 0 Then error(11)
              Else
                {如果在符号表中查不到, 则出错; 否则,做以下工作}
                With table[i] Do
                  If kind = Procedure2 Then
                    {如果在符号表中id是过程名}
                    gen(cal, lev-level, adr)
                    {生成一条过程调用指令;
                      lev: 当前语句所在过程的层号
                      level: 定义过程名id的层号;
                      adr: 过程id的代码中第一条指令的地址}
                  Else error(15); {若id不是过程名,则出错}
              getsym {取下一记号}
            End
        End

      Else If sym = ifsym Then {处理条件语句}
        Begin
          getsym; {取下一记号}
          condition([thensym, dosym]+fsys); {处理条件表达式}
          If sym = thensym Then getsym
          Else error(16);
          {如果当前记号是“then”,则取下一记号; 否则出错}
          cx1 := cx; {cx1记录下一代码的地址}
          gen(jpc, 0, 0);
          {生成指令,表达式为“假”转到某地址(待填),
          否则顺序执行}
          statement(fsys); {处理一个语句}
          code[cx1].a := cx
          {将下一个指令的地址回填到上面的jpc指令地址栏}
        End
      
      Else If sym = beginsym Then {处理语句序列}
        Begin
          getsym;
          statement([semicolon, endsym]+fsys);
          {取下一记号, 处理第一个语句}
          While sym In [semicolon]+statbegsys Do
          {如果当前记号是分号或语句的开始符号,则做以下工作}
            Begin
              If sym = semicolon Then getsym
              Else error(10);
              {如果当前记号是分号,则取下一记号, 否则出错}
              statement([semicolon, endsym]+fsys) {处理下一个语句}
            End;
          If sym = endsym Then getsym
          Else error(17)
          {如果当前记号是“end”,则取下一记号,否则出错}
        End

      Else If sym = whilesym Then {处理循环语句}
        Begin
          cx1 := cx; {cx1记录下一指令地址,即条件表达式的
                      第一条代码的地址}
          getsym; {取下一记号}
          condition([dosym]+fsys); {处理条件表达式}
          cx2 := cx; {记录下一指令的地址}
          gen(jpc, 0, 0); {生成一条指令,表达式为“假”转到某地
                            址(待回填), 否则顺序执行}
          If sym = dosym Then getsym
          Else error(18);
          {如果当前记号是“do”,则取下一记号, 否则出错}
          statement(fsys); {处理“do”后面的语句}
          gen(jmp, 0, cx1); {生成无条件转移指令, 转移到“while”后的
                              条件表达式的代码的第一条指令处}
          code[cx2].a := cx
          {把下一指令地址回填到前面生成的jpc指令的地址栏}
        End

      {###################### read语句 ######################}
      Else If sym = readsym Then {处理read语句}
        Begin
          getsym; {取下一记号}
          If sym = lparen Then {如果read后跟的是左括号}
            Repeat
              getsym;
              If sym = ident Then
                Begin
                  i := position(id);
                  {在符号表中查id, 返回id在符号表中的入口}
                  If i = 0 Then error(11)
                  {若在符号表中查不到id, 则出错, 否则做以下工作}
                  Else If table[i].kind <> variable Then
                    {若标识符id不是变量, 则出错}
                    Begin {对非变量赋值}
                      error(12);
                      i := 0;
                    End
                  Else With table[i] Do gen(red, lev-level, adr)
                    {生成一条RED指令;
                      lev: 当前语句所在过程的层号;
                      level: 定义变量id的过程的层号;
                      adr: 变量id在其过程的数据空间的相对地址}
                End
              Else error(4); {如果变量说明未出现标识符,则出错}
              getsym;
            Until sym <> comma {直到当前记号不是逗号}
          Else error(40); {如果read后跟的不是左括号,出错}
          If sym <> rparen Then error(22); {漏右括号,出错}
          getsym
        End {处理read语句}   

      {###################### write语句 ######################}
      Else if sym = writesym Then {处理write语句}
        Begin
          getsym; {取下一记号}
          If sym = lparen Then {如果write后跟的是左括号}
            Begin
              Repeat
                getsym;
                expression([rparen,comma]+fsys);
                gen(wrt, 0, 0);
              Until sym <> comma; {直到当前记号不是逗号}
              If sym <> rparen Then error(22); {漏右括号,出错}
              getsym
            End
          Else error(40) {如果write后跟的不是左括号,出错}
        End; {处理write语句}
            
      test(fsys, [ ], 19)
      {测试下一记号是否正常, 否则出错, 跳过一些记号}
    End {statement};

  Begin {block}
    dx := 3; {本过程数据空间栈顶指针}
    tx0 := tx; {标识符表的长度(当前指针)}
    table[tx].adr := cx; {本过程名的地址, 即下一条指令的序号}
    gen(jmp, 0, 0); {生成一条转移指令}
    If lev > levmax Then error(32);
    {如果当前过程层号>最大层数, 则出错}
    Repeat
      If sym = constsym Then {处理常数说明语句}
        Begin
          getsym;
          Repeat
            constdeclaration; {处理一个常数说明}
            While sym = comma Do {如果当前记号是逗号}
              Begin
                getsym;
                constdeclaration
              End; {处理下一个常数说明}
            If sym = semicolon Then getsym
            Else error(5)
            {如果当前记号是分号,则常数说明已处理完, 否则出错}
          Until sym <> ident
          {跳过一些记号, 直到当前记号不是标识符(出错时才用到)}
        End;

      If sym = varsym Then {当前记号是变量说明语句开始符号}
        Begin
          getsym;
          Repeat
            vardeclaration; {处理一个变量说明}
            While sym = comma Do {如果当前记号是逗号}
              Begin
                getsym;
                vardeclaration
              End;
            {处理下一个变量说明}
            If sym = semicolon Then getsym
            Else error(5)
            {如果当前记号是分号,则变量说明已处理完, 否则出错}
          Until sym <> ident;
          {跳过一些记号, 直到当前记号不是标识符(出错时才用到)}
        End;
      While sym = procsym Do {处理过程说明}
        Begin
          getsym;
          If sym = ident Then {如果当前记号是过程名}
            Begin
              enter(Procedure2);
              getsym
            End
          {把过程名填入符号表}
          Else error(4); {否则, 缺少过程名出错}
          If sym = semicolon Then getsym
          Else error(5);
          {当前记号是分号, 则取下一记号,否则,过程名后漏掉分号出错}
          block(lev+1, tx, [semicolon]+fsys); {处理过程体}
          {lev+1: 过程嵌套层数加1; tx: 符号表当前栈顶指针,
            也是新过程符号表起始位置; [semicolon]+fsys: 过程体开始和末尾符号集}
          If sym = semicolon Then {如果当前记号是分号}
            Begin
              getsym; {取下一记号}
              test(statbegsys+[ident, procsym], fsys, 6)
              {测试当前记号是否语句开始符号或过程说明开始符号,
                否则报告错误6, 并跳过一些记号}
            End
          Else error(5) {如果当前记号不是分号,则出错}
        End; {while}
      test(statbegsys+[ident], declbegsys, 7)
      {检测当前记号是否语句开始符号, 否则出错, 并跳过一些记号}
    Until not (sym In declbegsys);
    {回到说明语句的处理(出错时才用),直到当前记号不是说明语句的开始符号}
    code[table[tx0].adr].a := cx;
    {table[tx0].adr是本过程名的第1条
      代码(jmp, 0, 0)的地址,本语句即是将下一代码(本过程语句的第
      1条代码)的地址回填到该jmp指令中,得(jmp, 0, cx)}
    With table[tx0] Do {本过程名的第1条代码的地址改为下一指令地址cx}
      Begin
        adr := cx; {代码开始地址}
      End;
    cx0 := cx; {cx0记录起始代码地址}
    gen(int, 0, dx); {生成一条指令, 在栈顶为本过程留出数据空间}
    statement([semicolon, endsym]+fsys); {处理一个语句}
    gen(opr, 0, 0); {生成返回指令}
    test(fsys, [ ], 8); {测试过程体语句后的符号是否正常,否则出错}
    listcode; {打印本过程的中间代码序列}
  End  {block};



Procedure  interpret;
  Const stacksize = 500; {运行时数据空间(栈)的上界}
  Var p, b, t : integer; {程序地址寄存器, 基地址寄存器,栈顶地址寄存器}
      i : instruction; {指令寄存器}
      s : array [1..stacksize] Of integer; {数据存储栈}

  Function  base(l : integer) : integer;
    Var  b1 : integer;
    Begin
      b1 := b; {顺静态链求层差为l的外层的基地址}
      While l > 0 Do
        Begin
          b1 := s[b1];
          l := l-1
        End;
      base := b1
    End {base};

  Begin
    writeln('START PL/0');
    writeln(fout, 'START PL/0');
    t := 0; {栈顶地址寄存器}
    b := 1; {基地址寄存器}
    p := 0; {程序地址寄存器}
    s[1] := 0;
    s[2] := 0;
    s[3] := 0;
    {最外层主程序数据空间栈最下面预留三个单元}
    {每个过程运行时的数据空间的前三个单元是:SL, DL, RA;
    SL: 指向本过程静态直接外层过程的SL单元;
    DL: 指向调用本过程的过程的最新数据空间的第一个单元;
    RA: 返回地址 }
    Repeat
      i := code[p]; {i取程序地址寄存器p指示的当前指令}
      p := p+1; {程序地址寄存器p加1,指向下一条指令}
      With i Do
        Case f Of 
          lit :
            Begin {当前指令是取常数指令(lit, 0, a)}
              t := t+1;
              s[t] := a
            End;
            {栈顶指针加1, 把常数a取到栈顶}
          opr : Case a Of {当前指令是运算指令(opr, 0, a)}
            0 :
              Begin {a=0时,是返回调用过程指令}
                t := b-1; {恢复调用过程栈顶}
                p := s[t+3]; {程序地址寄存器p取返回地址}
                b := s[t+2]; {基地址寄存器b指向调用过程的基地址}
              End;
            1 : s[t] := -s[t]; {一元负运算, 栈顶元素的值反号}
            2 :
              Begin {加法}
                t := t-1;
                s[t] := s[t] + s[t+1]
              End;
            3 :
              Begin {减法}
                t := t-1;
                s[t] := s[t]-s[t+1]
              End;
            4 :
              Begin {乘法}
                t := t-1;
                s[t] := s[t] * s[t+1]
              End;
            5 :
              Begin {整数除法}
                t := t-1;
                s[t] := s[t] Div s[t+1]
              End;
            6 : s[t] := ord(odd(s[t])); 
              {算s[t]是否奇数, 是则s[t]=1, 否则s[t]=0}
            8 :
              Begin
                t := t-1;
                s[t] := ord(s[t] = s[t+1])
              End;
              {判两个表达式的值是否相等,是则s[t]=1,否则s[t]=0}
            9:
              Begin
                t := t-1;
                s[t] := ord(s[t] <> s[t+1])
              End;
              {判两个表达式的值是否不等,是则s[t]=1,否则s[t]=0}
            10 :
              Begin
                t := t-1;
                s[t] := ord(s[t] < s[t+1])
              End;
              {判前一表达式是否小于后一表达式,是则s[t]=1,否则s[t]=0}
            11:
              Begin
                t := t-1;
                s[t] := ord(s[t] >= s[t+1])
              End;
              {判前一表达式是否大于或等于后一表达式,是则s[t]=1,否则s[t]=0}
            12 :
              Begin
                t := t-1;
                s[t] := ord(s[t] > s[t+1])
              End;
              {判前一表达式是否大于后一表达式,是则s[t]=1,否则s[t]=0}
            13 :
              Begin
                t := t-1;
                s[t] := ord(s[t] <= s[t+1])
              End;
              {判前一表达式是否小于或等于后一表达式,是则s[t]=1,否则s[t]=0}
            End;
          lod :
            Begin {当前指令是取变量指令(lod, l, a)}
              t := t + 1;
              s[t] := s[base(l) + a]
              {栈顶指针加1, 根据静态链SL,将层差为l,
                相对地址为a的变量值取到栈顶}
            End;
          sto :
            Begin {当前指令是保存变量值(sto, l, a)指令}
              s[base(l) + a] := s[t];
              writeln(s[t]);
              writeln(fout, s[t]);
              {根据静态链SL,将栈顶的值存入层差为l,
                相对地址为a的变量中}
              t := t-1 {栈顶指针减1}
            End;
          cal :
            Begin {当前指令是(cal, l, a)}
              {为被调用过程数据空间建立连接数据}
              s[t+1] := base( l );
              {根据层差l找到本过程的静态直接外层过程的数据空间的SL单元,
                将其地址存入本过程新的数据空间的SL单元}
              s[t+2] := b; {调用过程的数据空间的起始地址存入本过程DL单元}
              s[t+3] := p; {调用过程cal指令的下一条的地址存入本过程RA单元}
              b := t+1; {b指向被调用过程新的数据空间起始地址}
              p := a {指令地址寄存储器指向被调用过程的地址a}
            End;
          int : t := t + a;
            {若当前指令是(int, 0, a), 则数据空间栈顶留出a大小的空间}
          jmp : p := a;
            {若当前指令是(jmp, 0, a), 则程序转到地址a执行}
          jpc :
            Begin {当前指令是(jpc, 0, a)}
              If s[t] = 0 Then p := a;
              {如果当前运算结果为“假”(0),程序转到地址a执行,否则顺序执行}
              t := t-1 {数据栈顶指针减1}
            End;

          red :
            Begin {当前指令是red}
              writeln('Input an integer: ');
              readln(s[base(l)+a]); {读一行数据,读入到层差为l,相对地址为a的变量值}
              writeln('Input: ', s[base(l)+a]);
              writeln(fout, 'Input: ', s[base(l)+a]);
            End;
          wrt :
            Begin {当前指令是wrt}
              writeln('Output: ', s[t]);
              writeln(fout, 'Output: ', s[t]);
              t := t+1 {数据栈顶指针加1}
            End
        End {with, case}
    Until p = 0;
    {程序一直执行到p取最外层主程序的返回地址0时为止}
    write('End PL/0');
    write(fout, 'End PL/0');
  End {interpret};


Begin  {主程序}
  writeln('Input PL0 src file name: ');
  readln(srcfile);
  assign(fin, srcfile); {将文件名字符串str赋给文件变量fin,
                        程序对文件变量fin的操作代替对文件str的操作}
  reset(fin); {打开文件}

  writeln('Input dst file name: ');
  readln(dstfile);
  assign(fout, dstfile);
  rewrite(fout); {新建文件(如果文件已经存在则冲掉)}

  For ch := 'a' To ';' Do ssym[ch] := nul;
  {ASCII码的顺序}
  {注意前面(二分查找)找关键字是按ASCII码顺序来找的,
   所以下面的关键字必须是ASCII码的顺序}
  word[1] := 'begin     ';
  word[2] := 'call      ';
  word[3] := 'const     ';
  word[4] := 'do        ';
  word[5] := 'end       ';
  word[6] := 'if        ';
  word[7] := 'odd       ';
  word[8] := 'procedure ';
  word[9] := 'read      ';
  word[10]:= 'then      ';
  word[11]:= 'var       ';
  word[12]:= 'while     ';
  word[13]:= 'write     ';

  wsym[1] := beginsym;
  wsym[2] := callsym;
  wsym[3] := constsym;
  wsym[4] := dosym;
  wsym[5] := endsym;
  wsym[6] := ifsym;
  wsym[7] := oddsym;
  wsym[8] := procsym;
  wsym[9] := readsym;
  wsym[10]:= thensym;
  wsym[11]:= varsym;
  wsym[12]:= whilesym;
  wsym[13]:= writesym;

  ssym['+'] := plus;
  ssym['-'] := minus;
  ssym['*'] := times;
  ssym['/'] := slash;
  ssym['('] := lparen;     
  ssym[')'] := rparen;
  ssym['='] := eql;
  ssym[','] := comma;
  ssym['.'] := period;
  ssym['!'] := neq; {不等号用!表示}
  ssym['<'] := lss;
  ssym['>'] := gtr;
  ssym['@'] := leq; {小于等于号用@表示}
  ssym['#'] := geq; {大于等于号用#表示}
  ssym[';'] := semicolon;
  {算符和标点符号的记号}

  mnemonic[lit] := '  LIT  ';
  mnemonic[opr] := '  OPR  ';
  mnemonic[lod] := '  LOD  ';
  mnemonic[sto] := '  STO  ';
  mnemonic[cal] := '  CAL  ';
  mnemonic[int] := '  INT  ';
  mnemonic[jmp] := '  JMP  ';
  mnemonic[jpc] := '  JPC  ';
  mnemonic[red] := '  RED  ';
  mnemonic[wrt] := '  WRT  ';
  {中间代码指令的字符串}

  declbegsys := [constsym, varsym, procsym];
  {说明语句的开始符号}
  statbegsys := [beginsym, callsym, ifsym, whilesym];
  {语句的开始符号}
  facbegsys := [ident, number, lparen];
  {因子的开始符号}

  // page(output);
  err := 0; {发现错误的个数}
  cc := 0; {当前行中输入字符的指针}
  cx := 0; {代码数组的当前指针}
  ll := 0; {输入当前行的长度}
  ch := ' '; {当前输入的字符}
  kk := al; {标识符的长度}
  getsym; {取下一个记号}
  block(0, 0, [period]+declbegsys+statbegsys); {处理程序体}
  If sym <> period Then error(9);
  {如果当前记号不是句号, 则出错}
  If err = 0 Then interpret
  {如果编译无错误, 则解释执行中间代码}
  Else 
    Begin
      write('ERRORS In PL/0 Program');
      write(fout, 'ERRORS In PL/0 Program');
    End;
  writeln;
  close(fin);
  readln(srcfile);
  close(fout);
End.
