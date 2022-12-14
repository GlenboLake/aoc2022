import re
import sys

simple = re.compile(r'\(\d+[*/+-]\d+\)')


def simplify(s):
    m = simple.search(s)
    while m is not None:
        s = s.replace(m.group(), str(int(eval(m.group()))))
        m = simple.search(s)
    return s


def sign(x):
    if x > 0:
        return 1
    elif x < 0:
        return -1
    return 0


def solve(eq):
    eq = simplify(eq)
    low, hi = 0, sys.maxsize
    low_sign = sign(eval(eq.replace('x', 'low')))
    while True:
        x = (low + hi) // 2
        result = eval(eq)
        if result == 0:
            return x
        if sign(result) == low_sign:
            low = x
        else:
            hi = x


haskell_output = '(((((14*6)+(((2*((((16+13)*2)+3)-18))*2)+(((3*7)*(3*(2+5)))+((13*13)+(((2+(9+(10+(2*5))))+((13*2)*3))+14)))))-(((1+(16*5))+(17*4))*2))+((((((2*((((((4*3)*2)+5)*((2*(5+12))/2))+((3*(((2*3)+17)*3))+((((((6+1)+3)-3)*3)+(12+(14+5)))+(19*3))))*2))/2)/2)+8)+((((((((5+(2*4))*(3+5))*((3*3)+(5*5)))+((((((2*(1+((2*3)*3)))/2)*11)+(((5*5)+12)*9))+(4+((5*(7+((6+7)*2)))-((2*13)*2))))*((5*2)+9)))*(19+((13*3)*2)))*2)*((2+(2*(2*4)))*((((2*5)*4)*((((((2*5)+(((5*4)+20)-7))*(1+6))+((3*(1+(2*(3+8))))*3))/4)+((3*(4+7))+((3*13)+(7*4)))))+(2*(((((((((5*2)+(3*7))*((4+2)*4))*(((((14+((8+9)+12))+(4*16))*2)/2)*3))/2)/2)+(((((((3*(((((2*((((1+9)+(((2*8)+(5*2))+5))+6)+(2+4)))*2)*2)+(((4*((2*3)+(9+((2*(2*11))/2))))+(2+((((2*(3*5))+11)*3)/3)))*2))+(7*(3*17))))*(5*2))-(((((((2+9)+6)+12)*12)-(2*(2*19)))-((2*13)+((5*5)+((4+(3+4))*2))))*(((4*(8*2))-(1+20))-2)))*4)-((9+(((1+(3*4))*4)*2))*(((((((4*2)*2)+1)*(4+3))+3)*8)/4)))-(((((((7+2)*7)+(8*17))+(2*(((4+2)+1)*((2*(14+(((11*((20+1)/3))+4)/3)))/2))))+(4*(((((3*10)+7)*(3+16))+((2*((((4+3)+6)*(2*((17*4)/4)))+(((2*(15+((2*(((2+5)*5)+(2*4)))+2)))/2)*7)))/2))/6)))*3)*3))-(7*((((((3+(6+(1+6)))*5)-(((6+(5*2))*((3*2)*2))/8))+(((11+12)*2)*((8*4)+(3*2))))+((2*(((((3*((6+1)*2))/(2+5))*(((3*3)*3)+(1+(5+4))))-(5*(7+6)))*5))+((3*7)+(10*(2+(1+5))))))-(((20*5)+((((((9+(16*2))+10)+(5*((2+4)+4)))+9)-1)*3))*2)))))/6)*3)))))-(((3+7)+(((((((5*5)*(5*((2*(2*7))/4)))+(((4+3)*((2+9)*4))+((((((2*(((((3*((2*(((2*((12*3)-10))*10)+((((4*4)+(3*(2+(9*3))))+4)+((7*4)*2))))/2))/3)+((((2*((((((2*((((((2*((((((((((((((((((2+(6+3))*(((3*(3*3))*(4+(2*(((12*2)/2)+1))))+((((((((((((8+5)*2)*3)+((2*(((2*8)*12)+1))/2))*3)+((((((3*((4+4)-1))+2)*3)/3)*4)*(((((9*(15+7))+((3*(5*3))+8))+((((x-((((9*3)*2)+((2*(2*(2+5)))+(5*5)))+((2*((1+6)*2))*((8+(3*2))+4))))*4)+((2*((((5*7)-4)*3)/3))*((9*2)-3)))/(9-3)))/2)-(((5+14)+14)*(3*2)))))+(11*((5*5)+18)))/3)-((3*3)*((2*15)+((3*7)+2))))*2)-(19*11))/(2+5))))-(3*(5+(((7*3)+(4*3))*2))))*2)+(((3*((3+(2*(2*(5*2))))*2))/2)*3))+(((5+2)+(4*(2*(((3*3)*5)+2))))+(14*(5*5))))/2)+(3*((6*(4+(3*((4*4)-2))))+((7*5)-4))))*2)-((19*2)*10))/(2*3))+((((2*4)*5)+(3+4))+((((((3*2)*(3*3))+(4+9))+((3*((9+2)*2))+((10+((2*4)*3))+(((2*(1+((2+(2*4))+5)))+1)+(2*4)))))+(9*3))+(((2*(9+20))/2)+(4*6)))))/(3*2))-(11*(13*5)))*((1+6)*3))+((((5*7)+(20+(14+((1+(1+8))+14))))+16)*4))/2)-((2*5)*(2*(1+((3*3)+3))))))-(2*((((2*((4+3)+(2*(2*(2*3)))))*2)+(3*5))+((((((3+14)*2)+(2*11))/(((3*2)+1)+1))+16)*8))))*2)+((8-1)*((5+16)+(2*(2*17)))))/3)-((3+(((2*(3+4))+(2*4))/2))+((((2+5)+((((((5*(((((((4+(4*2))/2)+3)*4)/3)*3)+1))+((2*3)*9))*2)/2)-((2*13)+3))/(4+3)))+(((3*4)+11)*2))+18))))+((5*(((((17*5)+(4*4))+(2*3))*2)/2))+((8*4)+(((6+1)*3)*((((1+20)/3)+1)-1)))))/2)+((3*((((7+(13*4))+(((9+1)-3)*2))*5)/5))+(((((5*17)-18)*2)+8)*4)))/2)-((18*15)+((((3*3)*2)*5)/3))))-((2*4)*(2*((2*(9+2))+(5+(5*2))))))*2)+((((3+(2*5))*4)+15)+((2*19)*14))))/2)-((5*((((1+((4*2)*2))*3)+2)+6))+(((2*((7*4)+(((2*(16+((5*((5+3)+5))/5)))+(5*5))+4)))+((11*4)/2))/2))))+(7*((2*((5*5)-3))+(((17+((2*3)*2))*3)/3))))/3)-((5*((3+4)+4))*5))*2)+(((3*(12-1))-(2*5))*(3*((8-2)+(5+2)))))))/(4*3))+(3*(((15*5)+((((3*3)*3)+((19*2)*2))+((3+4)*((3*2)+1))))+(1+(3*3)))))*3)-((((2*(((2+(11*5))-4)+((((3*((((5*(5+2))+2)*2)/2))-(((3*3)+(2*5))+3))*3)+((((2*(2+11))/2)*13)+(2+(((2+5)*2)+3))))))+((1+6)*(((2*(16+15))*(2*3))/3)))-(((2*(1+(6*6)))+(((2*14)+(3*((9+2)*3)))+(2*(((2*((4+2)+1))+3)+((2+9)+(5*5))))))*2))/2)))/7)))/2))*19)-((7*((((((((3*5)*2)+(8+((((3+(4+(((3*5)+(4*2))+11)))+(19+8))/2)-5)))+((3*12)-8))+((11*3)+(((5*5)+(11+1))-2)))*(((1+(11*2))*2)-((4+3)+2)))*(((2*3)+(5*(1+6)))*((13*(3+(5*2)))-(14*3))))+((((((((3*((((2*5)+13)+(2+((3*5)*2)))+((12+5)+(((4*4)-2)+(1+10)))))+((7+12)*4))*3)-((((4+7)+(3*3))+(((2*((2*(((11*3)+(6*3))-(2*7)))/2))*2)+(5*(3*3))))+((3*(3*3))*3)))+(5*(10+(5*5))))-(2+((((8+16)+2)+((((3*17)/3)+2)+(2*(4+8))))*3)))*(4*2))*((((((2*((3+4)+(11*2)))*2)*2)+(((((3+8)*5)+(8*4))+((((2*3)+(3*3))+((((2*3)*3)-5)+1))*4))+((((5*5)+(5*((4*4)+(3*2))))+((3*(7+4))+(((8+10)+7)-2)))+(13+(((6+(10*4))+1)*5)))))-((3*(3*17))/3))*6))))*((((((4*2)+(((2*5)-3)*3))+(3*3))/2)*((((5*2)-3)*((2*(13+1))/4))-(2*3)))*((2*((2*17)-7))+(2+5))))'

if __name__ == '__main__':
    print(solve(haskell_output))
