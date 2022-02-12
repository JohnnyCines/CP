\documentclass[a4paper]{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
\usepackage{palatino}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{graphicx}
\usepackage{cp2021t}
\usepackage{subcaption}
\usepackage{adjustbox}
\usepackage{color}
\definecolor{red}{RGB}{255,  0,  0}
\definecolor{blue}{RGB}{0,0,255}
\def\red{\color{red}}
\def\blue{\color{blue}}
%================= local x=====================================================%
\def\getGif#1{\includegraphics[width=0.3\textwidth]{cp2021t_media/#1.png}}
\let\uk=\emph
\def\aspas#1{``#1"}
%================= lhs2tex=====================================================%
%include polycode.fmt 
%format (div (x)(y)) = x "\div " y
%format succ = "\succ "
%format ==> = "\Longrightarrow "
%format map = "\map "
%format length = "\length "
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%format Left = "i_1"
%format Right = "i_2"
%format i1 = "i_1"
%format i2 = "i_2"
%format >< = "\times"
%format >|<  = "\bowtie "
%format |-> = "\mapsto"
%format . = "\comp "
%format .=?=. = "\mathbin{\stackrel{\mathrm{?}}{=}}"
%format (kcomp (f)(g)) = f "\kcomp " g
%format -|- = "+"
%format conc = "\mathsf{conc}"
%format summation = "{\sum}"
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (frac (a) (b)) = "\frac{" a "}{" b "}"
%format (uncurry f) = "\uncurry{" f "}"
%format (const f) = "\underline{" f "}"
%format TLTree = "\mathsf{TLTree}"
%format (lcbr (x)(y)) = "\begin{lcbr}" x "\\" y "\end{lcbr}"
%format (split (x) (y)) = "\conj{" x "}{" y "}"
%format (for (f) (i)) = "\for{" f "}\ {" i "}"
%format B_tree = "\mathsf{B}\mbox{-}\mathsf{tree} "
\def\ana#1{\mathopen{[\!(}#1\mathclose{)\!]}}
%format <$> = "\mathbin{\mathopen{\langle}\$\mathclose{\rangle}}"
%format (cataA (f) (g)) = "\cata{" f "~" g "}_A"
%format (anaA (f) (g)) = "\ana{" f "~" g "}_A"
%format (cataB (f) (g)) = "\cata{" f "~" g "}_B"
%format (cata (f)) = "\cata{" f "}"
%format (anaB (f) (g)) = "\ana{" f "~" g "}_B"
%format Either a b = a "+" b 
%format fmap = "\mathsf{fmap}"
%format NA   = "\textsc{na}"
%format NB   = "\textsc{nb}"
%format inT = "\mathsf{in}"
%format outT = "\mathsf{out}"
%format Null = "1"
%format (Prod (a) (b)) = a >< b
%format fF = "\fun F "
%format e1 = "e_1 "
%format e2 = "e_2 "
%format Dist = "\fun{Dist}"
%format IO = "\fun{IO}"
%format BTree = "\fun{BTree} "
%format LTree = "\mathsf{LTree}"
%format inNat = "\mathsf{in}"
%format (cataNat (g)) = "\cata{" g "}"
%format Nat0 = "\N_0"
%format Rational = "\Q "
%format toRational = " to_\Q "
%format fromRational = " from_\Q "
%format muB = "\mu "
%format (frac (n)(m)) = "\frac{" n "}{" m "}"
%format (fac (n)) = "{" n "!}"
%format (underbrace (t) (p)) = "\underbrace{" t "}_{" p "}"
%format matrix = "matrix"
%%format (bin (n) (k)) = "\Big(\vcenter{\xymatrix@R=1pt{" n "\\" k "}}\Big)"
%format `ominus` = "\mathbin{\ominus}"
%format % = "\mathbin{/}"
%format <-> = "{\,\leftrightarrow\,}"
%format <|> = "{\,\updownarrow\,}"
%format `minusNat`= "\mathbin{-}"
%format ==> = "\Rightarrow"
%format .==>. = "\Rightarrow"
%format .<==>. = "\Leftrightarrow"
%format .==. = "\equiv"
%format .<=. = "\leq"
%format .&&&. = "\wedge"
%format cdots = "\cdots "
%format pi = "\pi "
%format (curry (f)) = "\overline{" f "}"
%format (cataLTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (anaLTree (x)) = "\mathopen{[\!(}" x "\mathclose{)\!]}"
%format delta = "\Delta "

%---------------------------------------------------------------------------

\title{
       	Cálculo de Programas
\\
       	Trabalho Prático
\\
       	MiEI+LCC --- 2020/21
}

\author{
       	\dium
\\
       	Universidade do Minho
}


\date\mydate

\makeindex
\newcommand{\rn}[1]{\textcolor{red}{#1}}
\begin{document}

\maketitle

\begin{center}\large
\begin{tabular}{ll}
\textbf{Grupo} nr. & 23
\\\hline
a85481 & Bruno Alves 	
\\
a84684 & João Marques 
\\
a76964 & Luis Bigas 

\end{tabular}
\end{center}

\section{Preâmbulo}

\CP\ tem como objectivo principal ensinar
a progra\-mação de computadores como uma disciplina científica. Para isso
parte-se de um repertório de \emph{combinadores} que formam uma álgebra da
programação (conjunto de leis universais e seus corolários) e usam-se esses
combinadores para construir programas \emph{composicionalmente}, isto é,
agregando programas já existentes.
  
Na sequência pedagógica dos planos de estudo dos dois cursos que têm
esta disciplina, opta-se pela aplicação deste método à programação
em \Haskell\ (sem prejuízo da sua aplicação a outras linguagens 
funcionais). Assim, o presente trabalho prático coloca os
alunos perante problemas concretos que deverão ser implementados em
\Haskell.  Há ainda um outro objectivo: o de ensinar a documentar
programas, a validá-los e a produzir textos técnico-científicos de
qualidade.

\section{Documentação} Para cumprir de forma integrada os objectivos
enunciados acima vamos recorrer a uma técnica de programa\-ção dita
``\litp{literária}'' \cite{Kn92}, cujo princípio base é o seguinte:
%
\begin{quote}\em Um programa e a sua documentação devem coincidir.
\end{quote}
%
Por outras palavras, o código fonte e a documentação de um
programa deverão estar no mesmo ficheiro.

O ficheiro \texttt{cp2021t.pdf} que está a ler é já um exemplo de
\litp{programação literária}: foi gerado a partir do texto fonte
\texttt{cp2021t.lhs}\footnote{O suffixo `lhs' quer dizer
\emph{\lhaskell{literate Haskell}}.} que encontrará no
\MaterialPedagogico\ desta disciplina descompactando o ficheiro
\texttt{cp2021t.zip} e executando:
\begin{Verbatim}[fontsize=\small]
    $ lhs2TeX cp2021t.lhs > cp2021t.tex
    $ pdflatex cp2021t
\end{Verbatim}
em que \href{https://hackage.haskell.org/package/lhs2tex}{\texttt\LhsToTeX} é
um pre-processador que faz ``pretty printing''
de código Haskell em \Latex\ e que deve desde já instalar executando
\begin{Verbatim}[fontsize=\small]
    $ cabal install lhs2tex --lib
\end{Verbatim}
Por outro lado, o mesmo ficheiro \texttt{cp2021t.lhs} é executável e contém
o ``kit'' básico, escrito em \Haskell, para realizar o trabalho. Basta executar
\begin{Verbatim}[fontsize=\small]
    $ ghci cp2021t.lhs
\end{Verbatim}

%if False
\begin{code}
{-# OPTIONS_GHC -XNPlusKPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}
module Main where 
import Cp
import List hiding (fac)
import Nat
import LTree
import Data.List hiding (find)
import Test.QuickCheck hiding ((><),choose,collect)
import qualified Test.QuickCheck as QuickCheck
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Applicative hiding ((<|>))
import System.Process
\end{code}
%endif

\noindent Abra o ficheiro \texttt{cp2021t.lhs} no seu editor de texto preferido
e verifique que assim é: todo o texto que se encontra dentro do ambiente
\begin{quote}\small\tt
\verb!\begin{code}!
\\ ... \\
\verb!\end{code}!
\end{quote}
é seleccionado pelo \GHCi\ para ser executado.

\section{Como realizar o trabalho}
Este trabalho teórico-prático deve ser realizado por grupos de 3 (ou 4) alunos.
Os detalhes da avaliação (datas para submissão do relatório e sua defesa
oral) são os que forem publicados na \cp{página da disciplina} na \emph{internet}.

Recomenda-se uma abordagem participativa dos membros do grupo
de trabalho por forma a poderem responder às questões que serão colocadas
na \emph{defesa oral} do relatório.

Em que consiste, então, o \emph{relatório} a que se refere o parágrafo anterior?
É a edição do texto que está a ser lido, preenchendo o anexo \ref{sec:resolucao}
com as respostas. O relatório deverá conter ainda a identificação dos membros
do grupo de trabalho, no local respectivo da folha de rosto.

Para gerar o PDF integral do relatório deve-se ainda correr os comando seguintes,
que actualizam a bibliografia (com \Bibtex) e o índice remissivo (com \Makeindex),
\begin{Verbatim}[fontsize=\small]
    $ bibtex cp2021t.aux
    $ makeindex cp2021t.idx
\end{Verbatim}
e recompilar o texto como acima se indicou. Dever-se-á ainda instalar o utilitário
\QuickCheck,
que ajuda a validar programas em \Haskell\ e a biblioteca \gloss{Gloss} para
geração de gráficos 2D:
\begin{Verbatim}[fontsize=\small]
    $ cabal install QuickCheck gloss --lib
\end{Verbatim}
Para testar uma propriedade \QuickCheck~|prop|, basta invocá-la com o comando:
\begin{verbatim}
    > quickCheck prop
    +++ OK, passed 100 tests.
\end{verbatim}
Pode-se ainda controlar o número de casos de teste e sua complexidade,
como o seguinte exemplo mostra:
\begin{verbatim}
    > quickCheckWith stdArgs { maxSuccess = 200, maxSize = 10 } prop
    +++ OK, passed 200 tests.
\end{verbatim}
Qualquer programador tem, na vida real, de ler e analisar (muito!) código
escrito por outros. No anexo \ref{sec:codigo} disponibiliza-se algum
código \Haskell\ relativo aos problemas que se seguem. Esse anexo deverá
ser consultado e analisado à medida que isso for necessário.

\subsection{Stack}

O \stack{Stack} é um programa útil para criar, gerir e manter projetos em \Haskell.
Um projeto criado com o Stack possui uma estrutura de pastas muito específica:

\begin{itemize}
\item Os módulos auxiliares encontram-se na pasta \emph{src}.
\item O módulos principal encontra-se na pasta \emph{app}.
\item A lista de depêndencias externas encontra-se no ficheiro \emph{package.yaml}.
\end{itemize}

Pode aceder ao \GHCi\ utilizando o comando:
\begin{verbatim}
stack ghci
\end{verbatim}

Garanta que se encontra na pasta mais externa \textbf{do projeto}.
A primeira vez que correr este comando as depêndencias externas serão instaladas automaticamente.

Para gerar o PDF, garanta que se encontra na diretoria \emph{app}.

\Problema

Os \emph{tipos de dados algébricos} estudados ao longo desta disciplina oferecem
uma grande capacidade expressiva ao programador. Graças à sua flexibilidade,
torna-se trivial implementar \DSL s
e até mesmo \href{http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf}{linguagens de programação}.

Paralelamente, um tópico bastante estudado no âmbito de \DL\ 
é a derivação automática de expressões matemáticas, por exemplo, de derivadas.
Duas técnicas que podem ser utilizadas para o cálculo de derivadas são:

\begin{itemize}
\item \emph{Symbolic differentiation}
\item \emph{Automatic differentiation}
\end{itemize}

\emph{Symbolic differentiation} consiste na aplicação sucessiva de transformações
(leia-se: funções) que sejam congruentes com as regras de derivação. O resultado
final será a expressão da derivada.

O leitor atento poderá notar um problema desta técnica: a expressão
inicial pode crescer de forma descontrolada, levando a um cálculo pouco eficiente.
\emph{Automatic differentiation} tenta resolver este problema,
calculando \textbf{o valor} da derivada da expressão em todos os passos.
Para tal, é necessário calcular o valor da expressão \textbf{e} o valor da sua derivada.

Vamos de seguida definir uma linguagem de expressões matemáticas simples e
implementar as duas técnicas de derivação automática.
Para isso, seja dado o seguinte tipo de dados,

\begin{code}
data ExpAr a = X
           | N a
           | Bin BinOp (ExpAr a) (ExpAr a)
           | Un UnOp (ExpAr a)
           deriving (Eq, Show)
\end{code}

\noindent
onde |BinOp| e |UnOp| representam operações binárias e unárias, respectivamente:

\begin{code}
data BinOp = Sum
           | Product
           deriving (Eq, Show)

data UnOp = Negate
          | E
          deriving (Eq, Show)
\end{code}

\noindent
O construtor |E| simboliza o exponencial de base $e$.

Assim, cada expressão pode ser uma variável, um número, uma operação binária
aplicada às devidas expressões, ou uma operação unária aplicada a uma expressão.
Por exemplo,
\begin{spec}
Bin Sum X (N 10)
\end{spec}
designa |x+10| na notação matemática habitual.

\begin{enumerate}
\item A definição das funções |inExpAr| e |baseExpAr| para este tipo é a seguinte:
\begin{code}
inExpAr = either (const X) num_ops where
  num_ops = either N ops
  ops     = either bin (uncurry Un)
  bin(op, (a, b)) = Bin op a b

baseExpAr f g h j k l z = f -|- (g -|- (h >< (j >< k) -|- l >< z))
\end{code}

  Defina as funções |outExpAr| e |recExpAr|,
  e teste as propriedades que se seguem.
  \begin{propriedade}
    |inExpAr| e |outExpAr| são testemunhas de um isomorfismo,
    isto é,
    |inExpAr . outExpAr = id| e |outExpAr . idExpAr = id|:
\begin{code}
prop_in_out_idExpAr :: (Eq a) => ExpAr a -> Bool
prop_in_out_idExpAr = inExpAr . outExpAr .==. id

prop_out_in_idExpAr :: (Eq a) => OutExpAr a -> Bool
prop_out_in_idExpAr = outExpAr . inExpAr .==. id
\end{code}
    \end{propriedade}

  \item Dada uma expressão aritmética e um escalar para substituir o |X|,
	a função

\begin{quote}
      |eval_exp :: Floating a => a -> (ExpAr a) -> a|
\end{quote}

\noindent calcula o resultado da expressão. Na página \pageref{pg:P1}
    esta função está expressa como um catamorfismo. Defina o respectivo gene
    e, de seguida, teste as propriedades:
    \begin{propriedade}
       A função |eval_exp| respeita os elementos neutros das operações.
\begin{code}
prop_sum_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_sum_idr a exp = eval_exp a exp .=?=. sum_idr where
  sum_idr = eval_exp a (Bin Sum exp (N 0))

prop_sum_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_sum_idl a exp = eval_exp a exp .=?=. sum_idl where
  sum_idl = eval_exp a (Bin Sum (N 0) exp)

prop_product_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_product_idr a exp = eval_exp a exp .=?=. prod_idr where
  prod_idr = eval_exp a (Bin Product exp (N 1))

prop_product_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_product_idl a exp = eval_exp a exp .=?=. prod_idl where
  prod_idl = eval_exp a (Bin Product (N 1) exp)

prop_e_id :: (Floating a, Real a) => a -> Bool
prop_e_id a = eval_exp a (Un E (N 1)) == expd 1

prop_negate_id :: (Floating a, Real a) => a -> Bool
prop_negate_id a = eval_exp a (Un Negate (N 0)) == 0
\end{code}
    \end{propriedade}
    \begin{propriedade}
      Negar duas vezes uma expressão tem o mesmo valor que não fazer nada.
\begin{code}
prop_double_negate :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_double_negate a exp = eval_exp a exp .=?=. eval_exp a (Un Negate (Un Negate exp))
\end{code}
    \end{propriedade}

  \item É possível otimizar o cálculo do valor de uma expressão aritmética tirando proveito
  dos elementos absorventes de cada operação. Implemente os genes da função
\begin{spec}
      optmize_eval :: (Floating a, Eq a) => a -> (ExpAr a) -> a
\end{spec}
  que se encontra na página \pageref{pg:P1} expressa como um hilomorfismo\footnote{Qual é a vantagem de implementar a função |optimize_eval| utilizando um hilomorfismo em vez de utilizar um catamorfismo com um gene "inteligente"?}
  e teste as propriedades:

    \begin{propriedade}
      A função |optimize_eval| respeita a semântica da função |eval|.
\begin{code}
prop_optimize_respects_semantics :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_optimize_respects_semantics a exp = eval_exp a exp .=?=. optmize_eval a exp
\end{code}
    \end{propriedade}


\item Para calcular a derivada de uma expressão, é necessário aplicar transformações
à expressão original que respeitem as regras das derivadas:\footnote{%
	Apesar da adição e multiplicação gozarem da propriedade comutativa,
	há que ter em atenção a ordem das operações por causa dos testes.}

\begin{itemize}
  \item Regra da soma:
\begin{eqnarray*}
	\frac{d}{dx}(f(x)+g(x))=\frac{d}{dx}(f(x))+\frac{d}{dx}(g(x))
\end{eqnarray*}
  \item Regra do produto:
\begin{eqnarray*}
	\frac{d}{dx}(f(x)g(x))=f(x)\cdot \frac{d}{dx}(g(x))+\frac{d}{dx}(f(x))\cdot g(x)
\end{eqnarray*}
\end{itemize}

  Defina o gene do catamorfismo que ocorre na função
    \begin{quote}
      |sd :: Floating a => ExpAr a -> ExpAr a|
    \end{quote}
  que, dada uma expressão aritmética, calcula a sua derivada.
  Testes a fazer, de seguida:
    \begin{propriedade}
       A função |sd| respeita as regras de derivação.
\begin{code}
prop_const_rule :: (Real a, Floating a) => a -> Bool
prop_const_rule a = sd (N a) == N 0

prop_var_rule :: Bool
prop_var_rule = sd X == N 1

prop_sum_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool
prop_sum_rule exp1 exp2 = sd (Bin Sum exp1 exp2) == sum_rule where
  sum_rule = Bin Sum (sd exp1) (sd exp2)

prop_product_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool
prop_product_rule exp1 exp2 = sd (Bin Product exp1 exp2) == prod_rule where
  prod_rule =Bin Sum (Bin Product exp1 (sd exp2)) (Bin Product (sd exp1) exp2)

prop_e_rule :: (Real a, Floating a) => ExpAr a -> Bool
prop_e_rule exp = sd (Un E exp) == Bin Product (Un E exp) (sd exp)

prop_negate_rule :: (Real a, Floating a) => ExpAr a -> Bool
prop_negate_rule exp = sd (Un Negate exp) == Un Negate (sd exp)
\end{code}
    \end{propriedade}

\item Como foi visto, \emph{Symbolic differentiation} não é a técnica
mais eficaz para o cálculo do valor da derivada de uma expressão.
\emph{Automatic differentiation} resolve este problema cálculando o valor
da derivada em vez de manipular a expressão original.

  Defina o gene do catamorfismo que ocorre na função
    \begin{spec}
    ad :: Floating a => a -> ExpAr a -> a
    \end{spec}
  que, dada uma expressão aritmética e um ponto,
  calcula o valor da sua derivada nesse ponto,
  sem transformar manipular a expressão original.
  Testes a fazer, de seguida:

    \begin{propriedade}
       Calcular o valor da derivada num ponto |r| via |ad| é equivalente a calcular a derivada da expressão e avalia-la no ponto |r|.
\begin{code}
prop_congruent :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_congruent a exp = ad a exp .=?=. eval_exp a (sd exp)
\end{code}
    \end{propriedade}
\end{enumerate}

\Problema

Nesta disciplina estudou-se como fazer \pd{programação dinâmica} por cálculo,
recorrendo à lei de recursividade mútua.\footnote{Lei (\ref{eq:fokkinga})
em \cite{Ol18}, página \pageref{eq:fokkinga}.}

Para o caso de funções sobre os números naturais (|Nat0|, com functor |fF
X = 1 + X|) é fácil derivar-se da lei que foi estudada uma
	\emph{regra de algibeira}
	\label{pg:regra}
que se pode ensinar a programadores que não tenham estudado
\cp{Cálculo de Programas}. Apresenta-se de seguida essa regra, tomando como exemplo o
cálculo do ciclo-\textsf{for} que implementa a função de Fibonacci, recordar
o sistema
\begin{spec}
fib 0 = 1
fib(n+1) = f n

f 0 = 1
f (n+1) = fib n + f n
\end{spec}
Obter-se-á de imediato
\begin{code}
fib' = p1 . for loop init where
   loop(fib,f)=(f,fib+f)
   init = (1,1)
\end{code}
usando as regras seguintes:
\begin{itemize}
\item	O corpo do ciclo |loop| terá tantos argumentos quanto o número de funções mutuamente recursivas.
\item	Para as variáveis escolhem-se os próprios nomes das funções, pela ordem
que se achar conveniente.\footnote{Podem obviamente usar-se outros símbolos, mas numa primeira leitura
dá jeito usarem-se tais nomes.}
\item	Para os resultados vão-se buscar as expressões respectivas, retirando a variável |n|.
\item	Em |init| coleccionam-se os resultados dos casos de base das funções, pela mesma ordem.
\end{itemize}
Mais um exemplo, envolvendo polinómios do segundo grau $ax^2 + b x + c$ em |Nat0|.
Seguindo o método estudado nas aulas\footnote{Secção 3.17 de \cite{Ol18} e tópico
\href{https://www4.di.uminho.pt/~jno/media/cp/}{Recursividade mútua} nos vídeos das aulas teóricas.},
de $f\ x = a x^2 + b x + c$ derivam-se duas funções mutuamente recursivas:
\begin{spec}
f 0 = c
f (n+1) = f n + k n

k 0 = a + b
k(n+1) = k n + 2 a
\end{spec}
Seguindo a regra acima, calcula-se de imediato a seguinte implementação, em Haskell:
\begin{code}
f' a b c = p1 . for loop init where
  loop(f,k) = (f+k,k+2*a)
  init = (c,a+b) 
\end{code}
O que se pede então, nesta pergunta?
Dada a fórmula que dá o |n|-ésimo \catalan{número de Catalan},
\begin{eqnarray}
	C_n = \frac{(2n)!}{(n+1)! (n!) }
	\label{eq:cat}
\end{eqnarray}
derivar uma implementação de $C_n$ que não calcule factoriais nenhuns.
Isto é, derivar um ciclo-\textsf{for}
\begin{spec}
cat = cdots . for loop init where cdots
\end{spec}
que implemente esta função.

\begin{propriedade}
A função proposta coincidem com a definição dada:
\begin{code}
prop_cat = (>=0) .==>. (catdef  .==. cat)
\end{code}
\end{propriedade}
%
\textbf{Sugestão}: Começar por estudar muito bem o processo de cálculo dado
no anexo \ref{sec:recmul} para o problema (semelhante) da função exponencial.


\Problema 

As \bezier{curvas de Bézier}, designação dada em honra ao engenheiro
\href{https://en.wikipedia.org/wiki/Pierre_B%C3%A9zier}{Pierre Bézier},
são curvas ubíquas na área de computação gráfica, animação e modelação.
Uma curva de Bézier é uma curva paramétrica, definida por um conjunto
$\{P_0,...,P_N\}$ de pontos de controlo, onde $N$ é a ordem da curva.

\begin{figure}[h!]
  \centering
  \includegraphics[width=0.8\textwidth]{cp2021t_media/Bezier_curves.png}
  \caption{Exemplos de curvas de Bézier retirados da \bezier{ Wikipedia}.}
\end{figure}

O algoritmo de \emph{De Casteljau} é um método recursivo capaz de calcular
curvas de Bézier num ponto. Apesar de ser mais lento do que outras abordagens,
este algoritmo é numericamente mais estável, trocando velocidade por correção.

De forma sucinta, o valor de uma curva de Bézier de um só ponto $\{P_0\}$
(ordem $0$) é o próprio ponto $P_0$. O valor de uma curva de Bézier de ordem
$N$ é calculado através da interpolação linear da curva de Bézier dos primeiros
$N-1$ pontos e da curva de Bézier dos últimos $N-1$ pontos.

A interpolação linear entre 2 números, no intervalo $[0, 1]$, é dada pela
seguinte função:

\begin{code}
linear1d :: Rational -> Rational -> OverTime Rational
linear1d a b = formula a b where
  formula :: Rational -> Rational -> Float -> Rational
  formula x y t = ((1.0 :: Rational) - (toRational t)) * x + (toRational t) * y
\end{code}
%
A interpolação linear entre 2 pontos de dimensão $N$ é calculada através
da interpolação linear de cada dimensão.

O tipo de dados |NPoint| representa um ponto com $N$ dimensões.
\begin{code}
type NPoint = [Rational]
\end{code}
Por exemplo, um ponto de 2 dimensões e um ponto de 3 dimensões podem ser
representados, respetivamente, por:
\begin{spec}
p2d = [1.2, 3.4]
p3d = [0.2, 10.3, 2.4]
\end{spec}
%
O tipo de dados |OverTime a| representa um termo do tipo |a| num dado instante
(dado por um |Float|).
\begin{code}
type OverTime a = Float -> a
\end{code}
%
O anexo \ref{sec:codigo} tem definida a função 
    \begin{spec}
    calcLine :: NPoint -> (NPoint -> OverTime NPoint)
    \end{spec}
que calcula a interpolação linear entre 2 pontos, e a função
    \begin{spec}
    deCasteljau :: [NPoint] -> OverTime NPoint
    \end{spec}
que implementa o algoritmo respectivo.

\begin{enumerate}

\item Implemente |calcLine| como um catamorfismo de listas,
testando a sua definição com a propriedade:
    \begin{propriedade} Definição alternativa.
\begin{code}
prop_calcLine_def :: NPoint -> NPoint -> Float -> Bool
prop_calcLine_def p q d = calcLine p q d ==  zipWithM linear1d p q d
\end{code}
    \end{propriedade}

\item Implemente a função |deCasteljau| como um hilomorfismo, testando agora a propriedade:
    \begin{propriedade}
      Curvas de Bézier são simétricas.
\begin{code}
prop_bezier_sym :: [[Rational]] -> Gen Bool
prop_bezier_sym l = all (< delta) . calc_difs . bezs <$> elements ps  where
  calc_difs = (\(x, y) -> zipWith (\w v -> if w >= v then w - v else v - w) x y)
  bezs t    = (deCasteljau l t, deCasteljau (reverse l) (fromRational (1 - (toRational t))))
  delta = 1e-2
\end{code}
    \end{propriedade}

  \item Corra a função |runBezier| e aprecie o seu trabalho\footnote{%
        A representação em Gloss é uma adaptação de um
        \href{https://github.com/hrldcpr/Bezier.hs}{projeto}
        de Harold Cooper.} clicando na janela que é aberta (que contém, a verde, um ponto
        inicila) com o botão esquerdo do rato para adicionar mais pontos.
        A tecla |Delete| apaga o ponto mais recente.

\end{enumerate}

\Problema

Seja dada a fórmula que calcula a média de uma lista não vazia $x$,
\begin{equation}
avg\ x = \frac 1 k\sum_{i=1}^{k} x_i
\end{equation}
onde $k=length\ x$. Isto é, para sabermos a média de uma lista precisamos de dois catamorfismos: o que faz o somatório e o que calcula o comprimento a lista.
Contudo, é facil de ver que
\begin{quote}
	$avg\ [a]=a$
\\
	$avg (a:x) = \frac 1 {k+1}(a+\sum_{i=1}^{k} x_i) = \frac{a+k(avg\ x)}{k+1}$ para $k=length\ x$
\end{quote}
Logo $avg$ está em recursividade mútua com $length$ e o par de funções pode ser expresso por um único catamorfismo, significando que a lista apenas é percorrida uma vez.

\begin{enumerate}

\item	Recorra à lei de recursividade mútua para derivar a função
|avg_aux = cata (either b q)| tal que 
|avg_aux = split avg length| em listas não vazias. 

\item	Generalize o raciocínio anterior para o cálculo da média de todos os elementos de uma \LTree\ recorrendo a uma única travessia da árvore (i.e.\ catamorfismo).

\end{enumerate}
Verifique as suas funções testando a propriedade seguinte:
\begin{propriedade}
A média de uma lista não vazia e de uma \LTree\ com os mesmos elementos coincide,
a menos de um erro de 0.1 milésimas:
\begin{code}
prop_avg :: [Double] -> Property
prop_avg = nonempty .==>. diff .<=. const 0.000001 where
   diff l = avg l - (avgLTree . genLTree) l
   genLTree = anaLTree lsplit
   nonempty = (>[])
\end{code}
\end{propriedade}

\Problema	(\textbf{NB}: Esta questão é \textbf{opcional} e funciona como \textbf{valorização} apenas para os alunos que desejarem fazê-la.) 

\vskip 1em \noindent
Existem muitas linguagens funcionais para além do \Haskell, que é a linguagem usada neste trabalho prático. Uma delas é o \Fsharp\ da Microsoft. Na directoria \verb!fsharp! encontram-se os módulos \Cp, \Nat\ e \LTree\ codificados em \Fsharp. O que se pede é a biblioteca \BTree\ escrita na mesma linguagem.

Modo de execução: o código que tiverem produzido nesta pergunta deve ser colocado entre o \verb!\begin{verbatim}! e o \verb!\end{verbatim}! da correspondente parte do anexo \ref{sec:resolucao}. Para além disso, os grupos podem demonstrar o código na oral.

\newpage

\part*{Anexos}

\appendix

\section{Como exprimir cálculos e diagramas em LaTeX/lhs2tex}
Como primeiro exemplo, estudar o texto fonte deste trabalho para obter o
efeito:\footnote{Exemplos tirados de \cite{Ol18}.} 
\begin{eqnarray*}
\start
	|id = split f g|
%
\just\equiv{ universal property }
%
        |lcbr(
		p1 . id = f
	)(
		p2 . id = g
	)|
%
\just\equiv{ identity }
%
        |lcbr(
		p1 = f
	)(
		p2 = g
	)|
\qed
\end{eqnarray*}

Os diagramas podem ser produzidos recorrendo à \emph{package} \LaTeX\ 
\href{https://ctan.org/pkg/xymatrix}{xymatrix}, por exemplo: 
\begin{eqnarray*}
\xymatrix@@C=2cm{
    |Nat0|
           \ar[d]_-{|cataNat g|}
&
    |1 + Nat0|
           \ar[d]^{|id + (cataNat g)|}
           \ar[l]_-{|inNat|}
\\
     |B|
&
     |1 + B|
           \ar[l]^-{|g|}
}
\end{eqnarray*}

\section{Programação dinâmica por recursividade múltipla}\label{sec:recmul}
Neste anexo dão-se os detalhes da resolução do Exercício \ref{ex:exp} dos apontamentos da
disciplina\footnote{Cf.\ \cite{Ol18}, página \pageref{ex:exp}.},
onde se pretende implementar um ciclo que implemente
o cálculo da aproximação até |i=n| da função exponencial $exp\ x = e^x$,
via série de Taylor:
\begin{eqnarray}
	exp\ x 
& = &
	\sum_{i=0}^{\infty} \frac {x^i} {i!}
\end{eqnarray}
Seja $e\ x\ n = \sum_{i=0}^{n} \frac {x^i} {i!}$ a função que dá essa aproximação.
É fácil de ver que |e x 0 = 1| e que $|e x (n+1)| = |e x n| + \frac {x^{n+1}} {(n+1)!}$.
Se definirmos $|h x n| = \frac {x^{n+1}} {(n+1)!}$ teremos |e x| e |h x| em recursividade
mútua. Se repetirmos o processo para |h x n| etc obteremos no total três funções nessa mesma
situação:
\begin{spec}
e x 0 = 1
e x (n+1) = h x n + e x n

h x 0 = x
h x (n+1) = x/(s n) * h x n

s 0 = 2
s (n+1) = 1 + s n
\end{spec}
Segundo a \emph{regra de algibeira} descrita na página \ref{pg:regra} deste enunciado,
ter-se-á, de imediato:
\begin{code}
e' x = prj . for loop init where
     init = (1,x,2)
     loop(e,h,s)=(h+e,x/s*h,1+s)
     prj(e,h,s) = e
\end{code}

\section{Código fornecido}\label{sec:codigo}

\subsection*{Problema 1}

\begin{code}
expd :: Floating a => a -> a
expd = Prelude.exp

type OutExpAr a = Either () (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a)))
\end{code}

\subsection*{Problema 2}
Definição da série de Catalan usando factoriais (\ref{eq:cat}):
\begin{code}
catdef n = div (fac((2*n))) ((fac((n+1))*(fac n)))
\end{code}
Oráculo para inspecção dos primeiros 26 números de Catalan\footnote{Fonte:
\catalan{Wikipedia}.}:
\begin{code}
oracle = [
    1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845,
    35357670, 129644790, 477638700, 1767263190, 6564120420, 24466267020,
    91482563640, 343059613650, 1289904147324, 4861946401452
    ]
\end{code}

\subsection*{Problema 3}
Algoritmo:
\begin{spec}
deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau [] = nil
deCasteljau [p] = const p
deCasteljau l = \pt -> (calcLine (p pt) (q pt)) pt where
  p = deCasteljau (init l)
  q = deCasteljau (tail l)
\end{spec}
Função auxiliar:
\begin{spec}
calcLine :: NPoint -> (NPoint -> OverTime NPoint)
calcLine [] = const nil
calcLine(p:x) = curry g p (calcLine x) where
   g :: (Rational, NPoint -> OverTime NPoint) -> (NPoint -> OverTime NPoint)
   g (d,f) l = case l of
       []     -> nil
       (x:xs) -> \z -> concat $ (sequenceA [singl . linear1d d x, f xs]) z
\end{spec}
2D:
\begin{code}
bezier2d :: [NPoint] -> OverTime (Float, Float)
bezier2d [] = const (0, 0)
bezier2d l = \z -> (fromRational >< fromRational) . (\[x, y] -> (x, y)) $ ((deCasteljau l) z)
\end{code}
Modelo:
\begin{code}
data World = World { points :: [NPoint]
                   , time :: Float
                   }
initW :: World
initW = World [] 0

tick :: Float -> World -> World
tick dt world = world { time=(time world) + dt }

actions :: Event -> World -> World
actions (EventKey (MouseButton LeftButton) Down _ p) world =
  world {points=(points world) ++ [(\(x, y) -> map toRational [x, y]) p]}
actions (EventKey (SpecialKey KeyDelete) Down _ _) world =
    world {points = cond (== []) id init (points world)}
actions _ world = world

scaleTime :: World -> Float
scaleTime w = (1 + cos (time w)) / 2

bezier2dAtTime :: World -> (Float, Float)
bezier2dAtTime w = (bezier2dAt w) (scaleTime w)

bezier2dAt :: World -> OverTime (Float, Float)
bezier2dAt w = bezier2d (points w)

thicCirc :: Picture
thicCirc = ThickCircle 4 10

ps :: [Float]
ps = map fromRational ps' where
  ps' :: [Rational]
  ps' = [0, 0.01..1] -- interval
\end{code}
Gloss:
\begin{code}
picture :: World -> Picture
picture world = Pictures
  [ animateBezier (scaleTime world) (points world)
  , Color white . Line . map (bezier2dAt world) $ ps
  , Color blue . Pictures $ [Translate (fromRational x) (fromRational y) thicCirc | [x, y] <- points world]
  , Color green $ Translate cx cy thicCirc
  ] where
  (cx, cy) = bezier2dAtTime world
\end{code}
Animação:
\begin{code}
animateBezier :: Float -> [NPoint] -> Picture
animateBezier _ [] = Blank
animateBezier _ [_] = Blank
animateBezier t l = Pictures
  [ animateBezier t (init l)
  , animateBezier t (tail l)
  , Color red . Line $ [a, b]
  , Color orange $ Translate ax ay thicCirc
  , Color orange $ Translate bx by thicCirc
  ] where
  a@(ax, ay) = bezier2d (init l) t
  b@(bx, by) = bezier2d (tail l) t
\end{code}
Propriedades e \emph{main}:
\begin{code}
runBezier :: IO ()
runBezier = play (InWindow "Bézier" (600, 600) (0,  0))
  black 50 initW picture actions tick

runBezierSym :: IO ()
runBezierSym = quickCheckWith (stdArgs {maxSize = 20, maxSuccess = 200} ) prop_bezier_sym
\end{code}

Compilação e execução dentro do interpretador:\footnote{Pode ser útil em testes
envolvendo \gloss{Gloss}. Nesse caso, o teste em causa deve fazer parte de uma função
|main|.}
\begin{code}
main = runBezier

run = do { system "ghc cp2021t" ; system "./cp2021t" }
\end{code}

\subsection*{QuickCheck}
Código para geração de testes:
\begin{code}
instance Arbitrary UnOp where
  arbitrary = elements [Negate, E]

instance Arbitrary BinOp where
  arbitrary = elements [Sum, Product]

instance (Arbitrary a) => Arbitrary (ExpAr a) where
  arbitrary = do
    binop <- arbitrary
    unop  <- arbitrary
    exp1  <- arbitrary
    exp2  <- arbitrary
    a     <- arbitrary

    frequency . map (id >< pure) $ [(20, X), (15, N a), (35, Bin binop exp1 exp2), (30, Un unop exp1)]


infixr 5 .=?=.
(.=?=.) :: Real a => a -> a -> Bool
(.=?=.) x y = (toRational x) == (toRational y)


\end{code}

\subsection*{Outras funções auxiliares}
%----------------- Outras definições auxiliares -------------------------------------------%
Lógicas:
\begin{code}
infixr 0 .==>.
(.==>.) :: (Testable prop) => (a -> Bool) -> (a -> prop) -> a -> Property
p .==>. f = \a -> p a ==> f a

infixr 0 .<==>.
(.<==>.) :: (a -> Bool) -> (a -> Bool) -> a -> Property
p .<==>. f = \a -> (p a ==> property (f a)) .&&. (f a ==> property (p a))

infixr 4 .==.
(.==.) :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
f .==. g = \a -> f a == g a

infixr 4 .<=.
(.<=.) :: Ord b => (a -> b) -> (a -> b) -> (a -> Bool)
f .<=. g = \a -> f a <= g a

infixr 4 .&&&.
(.&&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&&&. g = \a -> ((f a) && (g a))
\end{code}

%----------------- Soluções dos alunos -----------------------------------------%

\section{Soluções dos alunos}\label{sec:resolucao}
Os alunos devem colocar neste anexo as suas soluções para os exercícios
propostos, de acordo com o "layout" que se fornece. Não podem ser
alterados os nomes ou tipos das funções dadas, mas pode ser adicionado
texto, disgramas e/ou outras funções auxiliares que sejam necessárias.

Valoriza-se a escrita de \emph{pouco} código que corresponda a soluções
simples e elegantes. 

\subsection*{Problema 1} \label{pg:P1}
São dadas:
\begin{code}
cataExpAr g = g . recExpAr (cataExpAr g) . outExpAr
anaExpAr g = inExpAr . recExpAr (anaExpAr g) . g
hyloExpAr h g = cataExpAr h . anaExpAr g

eval_exp :: Floating a => a -> (ExpAr a) -> a
eval_exp a = cataExpAr (g_eval_exp a)

optmize_eval :: (Floating a, Eq a) => a -> (ExpAr a) -> a
optmize_eval a = hyloExpAr (gopt a) clean

sd :: Floating a => ExpAr a -> ExpAr a
sd = p2 . cataExpAr sd_gen

ad :: Floating a => a -> ExpAr a -> a
ad v = p2 . cataExpAr (ad_gen v)
\end{code}
Definir:

\begin{code}
outExpAr  X = i1 ()
outExpAr (N a) = (i2 . i1) a
outExpAr (Bin op a b) =  (i2 . i2 . i1) (op, (a,b))
outExpAr (Un op a) =  (i2 . i2 . i2) (op, a)
---
recExpAr f = baseExpAr id id id f f id f
---
g_eval_exp x = either (const x) g1 where
    g1 = either (id) (ops) where
        ops = either binOp unOp where
            binOp (Sum, a) = uncurry(+) a
            binOp (Product, a) = uncurry(*) a
            unOp (Negate, a) = negate a
            unOp (E, a) = expd a
---
clean X = i1 ()
clean (N a) = (i2 . i1) a
clean (Bin Sum a b) = (i2 . i2 . i1) (Sum, (a,b))
clean (Bin Product a b)
  | a == (N 0) || b == (N 0) = (i2 . i1) 0
  | otherwise = (i2 . i2 .i1) (Product, (a,b))
clean (Un Negate a) = (i2 . i2 . i2) (Negate, a)
clean (Un E a)
  | a == (N 0) = (i2 . i1) 0
  | otherwise = (i2 . i2 . i2) (E, a)
---
gopt :: Floating a => a -> Either b (Either a (Either (BinOp, (a, a)) (UnOp, a))) -> a
gopt = g_eval_exp
\end{code}

\begin{code}
sd_gen :: Floating a =>
    Either () (Either a (Either (BinOp, ((ExpAr a, ExpAr a), (ExpAr a, ExpAr a))) (UnOp, (ExpAr a, ExpAr a)))) -> (ExpAr a, ExpAr a)
sd_gen = either fvar (either fconst fops)
  where
    fvar a = (X, N 1)
    fconst a = (N a, N 0)

    fops = either fbinOp funOp
      where
        fbinOp (Sum, ((a, b), (c, d))) = ((Bin Sum a c), (Bin Sum b d))
        fbinOp (Product, ((a, b), (c, d))) = ((Bin Product a c), (Bin Sum (Bin Product a d) (Bin Product b c)))

        funOp (Negate, (a, b)) = (Un Negate a, Un Negate b)
        funOp (E, (a, b)) = (Un E a, Bin Product (Un E a) b)
\end{code}

\begin{code}
ad_gen var = either (split (const var) (const 1)) g
  where
    g = either fcons fops
      where
        fcons x = (x, 0)
        fops = either fbinOp funOp
          where
            fbinOp (Sum, ((a, b), (c, d))) = ((+) a c, (+) b d)
            fbinOp (Product, ((a, b), (c, d))) = ((*) a c, (+) ((*) a d) ((*) b c))

            funOp (Negate, (a, b)) = (negate a, negate b)
            funOp (E, (a, b)) = (expd a, (*) (expd a) b)
\end{code}

\subsection*{Problema 2}
Definir
\begin{code}
catal 0 = 1
catal (n+1) = (*) (catal n) (div (f n) (k n))

f 0 = 2
f (n+1) = (f n) + (f2 n)

f2 0 = 10
f2 (n+1) = (f2 n) + 8

k 0 = 2
k (n+1) = (k n) + (k2 n)

k2 0 = 4
k2 (n+1) = (k2 n) + 2

loop (catal,f,f2,k,k2) = (div (catal * f) k,f+f2, f2+8, k+k2, k2+2)
inic = (1,2,10,2,4)
prj (catal,f,f2,k,k2)= catal
\end{code}
por forma a que
\begin{code}
cat = prj . (for loop inic)
\end{code}
seja a função pretendida.
\textbf{NB}: usar divisão inteira.
Apresentar de seguida a justificação da solução encontrada.

Partindo da fórmula que dá o n-ésimo número de Catalan:

\begin{eqnarray}
  C_n = \frac{(2n)!}{(n+1)!(n!)} \nonumber
\end{eqnarray}
Utilizamos a estratégia encontrada no anexo \ref{sec:recmul}, e começamos por calcular |C 0| e |C (n+1)|
\begin{eqnarray}
  C_0 = 1 \nonumber
\end{eqnarray}

\begin{eqnarray}
  C_{n+1} = \frac{(2(n+1))!}{((n+1)+1)!(n+1)!} \nonumber \\
  = \frac{(2n+2)!}{(n+2)!(n+1)!} \nonumber \\
  = \frac{(2n+2)(2n+1)(2n)!}{(n+2)(n+1)n!(n+1)!} \nonumber \\
  = \frac{(4n^{2}+6n+2)(2n)!}{(n^{2}+3n+2)(n+1)!n!} \nonumber \\
\end{eqnarray}

Partindo a equação em duas partes, conseguimos isolar a fórmula da qual partimos do número de Catalan .
\begin{eqnarray}
  C_{n+1} = C_n \times \frac{(4n^{2}+6n+2)}{(n^{2}+3n+2)} \nonumber \\
\end{eqnarray}
Ao contrário do exemplo encontrado no anexo \ref{sec:recmul} em que se obtinha |e x| e |h x| em recursividade
mútua, aqui é necessário definirmos duas funçoes á qual designaremos por |f| e |k| tal que :

\begin{eqnarray}
  f_n = 4n^{2}+6n+2 \nonumber
\end{eqnarray}

\begin{eqnarray}
  k_n = n^2 + 3n + 2 \nonumber
\end{eqnarray}

E portanto obtemos |f n| , |k n| e |C n| em recursividade mútua.


\begin{eqnarray}
  C_{n+1} = \frac{f_n}{k_n} \times C_n \nonumber
\end{eqnarray}

Continuando a seguir o exemplo do anexo, repetimos agora o processo para |f| e |k|.

Para |f|:
\begin{eqnarray}
  f_0 = 2 \nonumber
\end{eqnarray}

\begin{eqnarray}
  f_{n+1} = 4(n+1)^{2}+6(n+1)+2  \nonumber \\
  = 4(n^2 + 2n + 1) + 6n + 6 + 2 \nonumber \\
  = 4n^2 + 8n + 4 + 6n + 6 + 2 \nonumber \\
  = (4n^2 + 6n + 2) + (8n +10) \nonumber \\
\end{eqnarray}

Definindo |f2 n| = |8n + 10|, obtemos:

\begin{eqnarray}
  f_{n+1} = f_n + f2_n \nonumber
\end{eqnarray}

Para |k|:
\begin{eqnarray}
  k_0 = 2 \nonumber
\end{eqnarray}

\begin{eqnarray}
  k_{n+1} = (n+1)^2 + 3(n+1) + 2 \nonumber \\
  n^2 + 2n + 1 + 3n + 3 + 2 \nonumber \\
  (n^2 + 3n + 2) + (2n + 4) \nonumber \\
  k_n + k2_n \nonumber
\end{eqnarray}

Definindo |k2 n| = |2n + 4|, obtemos:

\begin{eqnarray}
  k_{n+1} = k_n + k2_n \nonumber
\end{eqnarray}


Desenvolvendo agora as funções |f2| e |k2|: 


\begin{eqnarray}
  f2_n = 8n + 10 \nonumber
\end{eqnarray}

\begin{eqnarray}
  f2_0 = 10 \nonumber
\end{eqnarray}

\begin{eqnarray}
  f2_{n+1} = 8(n+1) + 10 \nonumber \\
  8n + 8 + 10 \nonumber \\
  (8n + 10) + 8 \nonumber \\
  f2_n + 8 \nonumber
\end{eqnarray}



\begin{eqnarray}
  k2_n = 2n + 4 \nonumber
\end{eqnarray}

\begin{eqnarray}
  k2_0 = 4 \nonumber
\end{eqnarray}

\begin{eqnarray}
  k2_{n+1} = 2(n+1) + 4 \nonumber \\
  2n + 2 + 4 \nonumber \\
  (2n+4) + 2 \nonumber \\
  k2_n + 2 \nonumber
\end{eqnarray}

Terminado este processo obtemos 5 funções ( |C|, |f|, |f2|, |k|, |k2|). Utilizamos agora a \emph{regra de algibeira} descrita no enunciado e sabemos que:
\begin{itemize}
\item	O corpo do ciclo |loop| terá 5 argumentos .
\item	As variáveis serão |C|, |f|, |f2|, |k| e |k2|.
\item	Os resultados são retirados das expressões respectivas, retirando a variável |n|. 
\end{itemize}
Com isto podemos finalmente escrever o nosso |loop|:
\begin{spec}
loop (catal,f,f2,k,k2) = (div (catal * f) k,f+f2, f2+8, k+k2, k2+2)
\end{spec}

Olhando para a última parte da \emph{regra de algibeira} :
\begin{itemize}
\item	Em |init| coleccionam-se os resultados dos casos de base das funções |C|, |f|, |f2|, |k|, |k2|
\end{itemize}

Podemos concluir preenchendo o |init|:

\begin{spec}
inic = (1,2,10,2,4)
\end{spec}




\subsection*{Problema 3}

\begin{code}

calcLine :: NPoint -> (NPoint -> OverTime NPoint)
calcLine = cataList h where
  h = either (const$const nil) (k2) where
  k2 (q,g) [] f = []
  k2 (q,g) (x:xs) f = linear1d q x f: g xs f
---
data AlgForm a = Vazio | Unidade a | Par (AlgForm a) (AlgForm a)
---
inALgForm = either (const Vazio) (either (Unidade) (uncurry Par))
---
outAlgForm Vazio = i1()
outAlgForm (Unidade a) = (i2.i1) (a)
outAlgForm (Par x y)= (i2.i2) (x,y)
---
fAlgForm g = id -|- (id -|- (g><g))
---
anaAlgForm g = inALgForm . fAlgForm(anaAlgForm g) . g
---
cataAlgForm g = g . fAlgForm(cataAlgForm g) . outAlgForm
---
deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau = hyloAlgForm alg coalg where
   coalg [] = i1() 
   coalg [p] = (i2.i1) p
   coalg l = (i2.i2) (init l, tail l)
   alg  = either (a) (either b c) where
    a = const nil 
    b = const
    c = \(p,q) -> \(pt) -> (calcLine(p pt) (q pt))pt
    
---
hyloAlgForm f g = cataAlgForm f. anaAlgForm g

\end{code}

\begin{figure}
\includegraphics[width=0.5\linewidth]{media/d1.jpg}
\end{figure}

\begin{figure}
\includegraphics[width=0.5\linewidth]{media/d2.jpg}
\end{figure}

\subsection*{Problema 4}

Solução para listas não vazias:
\begin{code}
avg = p1.avg_aux
\end{code}

\begin{code}
myListin = either singl cons
---
myListout [a] = i1 a
myListout (h:t) = i2 (h,t)
---
cataMyList g = g . recMyList (cataMyList g) . myListout
---
recMyList f = id -|- id >< f
---
avg_aux = cataMyList gene
  where
    gene = either (split (id) (const 1)) (split h k)
      where
        h a = (/) ((+) (p1 a) ((*) ((p1 . p2) a) ((p2 . p2) a))) (succ $ (p2 . p2) a)
        k a = succ $ (p2 . p2) a
\end{code}

Como estamos a trabalhar com listas não vazias, definimos:

\begin{eqnarray*}
myList [a] = [a]\ \vert \ (a : [a])
\end{eqnarray*}

\begin{eqnarray*}
myListin = either\ singl\ cons
\end{eqnarray*}

\begin{eqnarray*}
myListout [a] = i1\  a \\
myListout (h:t) = i2\ (h,t)
\end{eqnarray*}


\begin{eqnarray*}
\start
	|split avg length = cata (split h k)|
%
\just\equiv{ Fokkinga }
%
        |lcbr(
		avg . in = h . F (split avg length)
	)(
		length . in = k . F (split avg length)
	)|
%
\just\equiv{ def-in, Funtor myList, h=[h1,h2], k=[k1,k2] }
%
        |lcbr(
		avg . (either singl cons) = (either h1 h2) . (id + id >< (split avg length))
	)(
        	length . (either singl cons) = (either k1 k2) . (id + id >< (split avg length))	
	)|
%
\just\equiv{ Fusão-+, Absorção-+, Natural-id }
%
        |lcbr(
                either( avg . singl, avg . cons) = either(h1, h2 . (id >< split(avg length)))
	)(
		either( length . singl, length . cons) = either(k1, k2 . (id >< split(avg length)))
	)|
%
\just\equiv{ Def-x, Natural-id }
%
        |lcbr(
                either( avg . singl, avg . cons) = either(h1, h2 . split(p1,  split(avg length) . p2))
	)(
		either( length . singl, length . cons) = either(k1, k2 . (id >< split(avg length)))
	)|
%
\just\equiv{ Fusão-x }
%
        |lcbr(
                either( avg . singl, avg . cons) = either(h1, h2 . split(avg . p2, length . p2))
	)(
		either( length . singl, length . cons) = either(k1, k2 . split(avg . p2, length . p2))
	)|
%
\just\equiv{ Eq-+ }
%
        |lcbr(
              avg . singl = h1
	)(
              avg . cons = h2 . split(avg . p2, length . p2)
        )|

        |lcbr(
              length . singl = k1
        )(
              length . cons = k2 . split(avg . p2, length . p2)
	)|
%
\just\equiv{ def avg, def length }
%
        |lcbr(
              avg . singl = id
	)(
              avg . cons = ((p1 + ((p1 . p2) * (p2 . p2))) / (succ . p2 . p2)) . split(avg . p2, length . p2)
        )|

        |lcbr(
              length . singl = const 1
        )(
              length . cons = (succ . p2 . p2) . split(avg . p2, length . p2)
	)|
%
\just\equiv{ Invertendo até ao primeiro passo}
%
        |lcbr(
          avg . either(singl cons) = h . F(split(avg length))
        )(
          length . either(singl cons) = k . F(split(avg length))
        )|
%
\just\equiv{ Fokkinga }
%
      	|split avg length = cata (split( either(h1, h2), either(k1, k2)))|
%
\just\equiv{ lei da troca, def h1, def h2, def k1, def k2 }
%
       	|split avg length = cata (either(split(id, const 1), split((p1 + ((p1 . p2) * (p2 . p2))) / (succ . p1 . p2), succ . p2 . p2)))|
\qed
\end{eqnarray*}

Solução para árvores de tipo \LTree:
\begin{code}
avgLTree = p1.cataLTree gene where
   gene = either (split (id) (const 1)) (split h k)
     where
       h a = (/) ((+) ((*) ((p1 . p1) a) ((p2 . p1) a)) ((*) ((p1 . p2) a) ((p2 . p2) a))) ((+) ((p2 . p1) a) ((p2 . p2) a))
       k a = (+) ((p2 . p1) a) ((p2 . p2) a)
\end{code}

No caso das LTrees, temos as seguintes definições:

\begin{spec}
length (Leaf a) = 1
length (Fork a b) = (length a) + (length b)
\end{spec}

\begin{spec}
avg (Leaf a) = a
avg (Fork a b) = (avg a * length a) + (avg b * length b) / (length a + length b)
\end{spec}

\begin{eqnarray*}
\start
	|split avg length = cata (split h k)|
%
\just\equiv{ Fokkinga }
%
        |lcbr(
		avg . in = h . F (split avg length)
	)(
		length . in = k . F (split avg length)
	)|
%
\just\equiv{ def-in, Funtor myList, h=[h1,h2], k=[k1,k2] }
%
        |lcbr(
		avg . (either Leaf Fork) = (either h1 h2) . (id +  (split avg length) >< (split avg length))
	)(
        	length . (either Leaf Fork) = (either k1 k2) . (id +  (split avg length) >< (split avg length))	
	)|
%
\just\equiv{ Fusão-+, Absorção-+, Natural-id }
%
        |lcbr(
                either( avg . Leaf, avg . Fork) = either(h1, h2 . ( split(avg length) >< split(avg length)))
	)(
		either( length . Leaf, length . Fork) = either(k1, k2 . (split(avg length) >< split(avg length)))
	)|
%
\just\equiv{ Def-x }
%
        |lcbr(
                either( avg . Leaf, avg . Fork) = either(h1, h2 . ( split(split(avg length) . p1, split(avg length) . p2)))
	)(
		either( length . Leaf, length . Fork) = either(h1, h2 . ( split(split(avg length) . p1, split(avg length) . p2)))
	)|
%
\just\equiv{ Eq-+ }
%
        |lcbr(
              avg . Leaf = h1
	)(
              avg . Fork = h2 . ( split(split(avg length) . p1, split(avg length) . p2))
        )|

        |lcbr(
              length . Leaf = k1
        )(
              length . Fork = k2 . ( split(split(avg length) . p1, split(avg length) . p2))
	)|
%
\just\equiv{ def avg, def length }
%
        |lcbr(
              avg . Leaf = id
	)(
              avg . Fork = ((p1 . p1)*(p2 . p1)+(p2.p2)*(p1.p2))/((p2.p1)+(p2.p2)) . ( split(split(avg length) . p1, split(avg length) . p2))
        )|

        |lcbr(
              length . Leaf = const 1
        )(
              length . Fork = ((p2.p1) + (p2 . p2)) . ( split(split(avg length) . p1, split(avg length) . p2))
	)|
%
\just\equiv{ Invertendo até ao primeiro passo}
%
        |lcbr(
          avg . either(Leaf Fork) = h . F(split(avg length))
        )(
          length . either(Leaf Fork) = k . F(split(avg length))
        )|
%
\just\equiv{ Fokkinga }
%
      	|split avg length = cata (split( either(h1 h2), either(k1 k2)))|

\qed
\end{eqnarray*}

\subsection*{Problema 5}
Inserir em baixo o código \Fsharp\ desenvolvido, entre \verb!\begin{verbatim}! e \verb!\end{verbatim}!:

\begin{verbatim}
module BTree

open Cp

// (1) Datatype definition -----------------------------------------------------
type BTree<'a> =  Empty | Node of 'a * (BTree<'a> * BTree<'a>)

let inLTree x = either (konst Empty) Node x

let outLTree x = match x with
        |Empty -> Left ()
        |Node (a,(t1,t2)) -> Right(a,(t1,t2))

// (2) Ana + cata + hylo -------------------------------------------------------

let baseBTree f g = id -|- (f >< (g >< g))

let recBTree f = baseBTree id g         // that is:  id -|- (f >< f)

let rec cataBTree a = g << (recBTree (cataBTree g)) << outBTree

let rec anaBTree f = inBTree << (recBTree (anaBTree g) ) << g

let hyloBTree a c = cataBTree h << anaBTree g

// (3) Map ---------------------------------------------------------------------

//instance Functor BTree
//         where fmap f = cataBTree ( inBTree . baseBTree f id )
let fmap f = cataBTree ( inBTree << baseBTree f id )

let fmap f = anaBTree ( baseBTree f id << outBTree )

// (4) Examples ----------------------------------------------------------------

// (4.1) Inversion (mirror) ----------------------------------------------------

let invBTree x = cataBTree (inBTree << (id -|- id >< swap)) x

// (4.2) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either (konst 0) (succ << (uncurry (+)) << p2)) x

// (4.3) Serialization ---------------------------------------------------------

let inordt x = cataBTree inord x                 // in-order traversal

// where

let inord x = 
        let join(x,(l,r)) = l @ [x] @ r
        in either nil join x

let preordt x = cataBTree preord x                  // pre-order traversal

let preord x =  
        let  f(x,(l,r)) = x :: l @ r
        in either nil f x

let postordt x = 
        let f(x,(l,r))=l @ r @ [x]
        in cataBTree (either nil f) x               // post-order traversal

// (4.4) Quicksort -------------------------------------------------------------

let qSort x = hyloBTree inord qsep x               // the same as (cataBTree inord) . (anaBTree qsep)

let qsep x = match x with
        |[] -> Left ()
        |(h :: t) -> let (s,l) = part (<h) t in Right (h,(s,l))

//let part x = ...
        
// (4.5) Traces ----------------------------------------------------------------

let traces x = cataBTree (either (konst [[]]) tunion) x

let tunion(a,(l,r)) x = union (map ((:) a)) l) (map ((:) a)) r) 

// (4.6) Towers of Hanoi -------------------------------------------------------
let hanoi x = hyloBTree present strategy x

let present x = inord x  //same as in qSort

let strategy x = match x with
            |(d,0) -> Left ()
            |(d,n+1) -> Right ((n,d),((not d,n),(not d,n)))

// -------------------------- end of library ----------------------------------
\end{verbatim}

%----------------- Fim do anexo com soluções dos alunos ------------------------%

%----------------- Índice remissivo (exige makeindex) -------------------------%

\printindex

%----------------- Bibliografia (exige bibtex) --------------------------------%

\bibliographystyle{plain}
\bibliography{cp2021t}

%----------------- Fim do documento -------------------------------------------%
\end{document}