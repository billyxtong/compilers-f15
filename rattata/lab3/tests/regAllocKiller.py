header = "//test return 666\n//Where is your god now?\n\n"
num = 666
args1 = ['int bloo' + str(i+1) for i in xrange(num)]
args1str = ", ".join(args1)
args2 = ['int blah' + str(i) for i in xrange(num)]
args2str = ", ".join(args2)
args1 = "(" + args1str + ")"
args2 = "(" + args2str + ")"
start = '666'
zeros = ", ".join([('666' if i > 0 else start) for i in xrange(num)])
main = "int main () {\n\treturn blooblah(" + zeros + ");\n}\n\n"
zeros = ", ".join([('0' if i > 0 else start) for i in xrange(num)])
argsForBloo = ['bloo0 - 1' if i == 0 else 'bloo'+str(i) for i in xrange(num)]
argsForBloo = ", ".join(argsForBloo)
argsForBlah = ['blah0 - 1' if i == 0 else 'blah'+str(i) for i in xrange(num)]
argsForBlah = ", ".join(argsForBlah)

bloodef = 'int blooblah ' + args1 + " {\n\t"
bloobody = "".join(['\t666;\n' for i in xrange(666)])
bloobody += "\t" + " + ". join(['bloo' + str(i+1) for i in xrange(num)])+ ";\n"
bloobody += '\treturn 666;\n}'
# bloobody = 'if (bloo0 == 0) return bloo0; else return blahbloo(' +argsForBloo + ");\n}\n\n" 
blahdef = 'int blahbloo ' + args2 + " {\n\t"
blahbody = 'if (blah0 == 0) return blah0; else return blooblah(' +argsForBlah + ");\n}\n\n"

decl1 = "int blooblah" + args1 + ";\n"
decl2 = "int blahbloo" + args2 + ";\n\n"
with open("test.l3", 'w') as f:
    f.write(header)
    f.write(decl1)
    f.write(main)
    f.write(bloodef)
    f.write(bloobody)
