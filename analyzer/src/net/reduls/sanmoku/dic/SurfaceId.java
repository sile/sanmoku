package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class SurfaceId {
    private static final int idOffset;
    private static final byte[] nodes;
    
    static {
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("surface-id.bin");
            
            final int nodeCount = Misc.readInt(in)/8;
            final int extCount = Misc.readInt(in)/4; // XXX: unused
            
            nodes = new byte[nodeCount*8];
            
            try {
                in.readFully(nodes, 0, nodes.length);
            } catch(Exception e) {}

            Misc.close(in);
        }
        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("category.bin");
            idOffset = Misc.readInt(in);
            Misc.close(in);
        }
    }
 
    public static void eachCommonPrefix(String text, int start, WordDic.Callback fn) {
        long node = getNode(0);
        int id = idOffset;

        final CodeStream in = new CodeStream(text,start);
        for(;;) {
            if(isTerminal(node))
                WordDic.eachViterbiNode(fn, id++, start, in.position()-start, false);
            
            if(in.isEos())
                return;
            
            if(checkEncodedChildren(in, node)==false)
                return;
            
            final char arc = in.read();
            final long next = getNode(base(node)+arc);
            if(chck(next) != arc)
                return;
            node = next;
            id += siblingTotal(node);
        }
    }

    private static boolean checkEncodedChildren(CodeStream in, long node) {
        switch(type(node)) {
        case 0:
            return checkEC(in,node,0) && checkEC(in,node,1);
        case 1:
            return checkEC(in,node,0);
        default:
            return true;
        }
    }
    private static boolean checkEC(CodeStream in, long node, int n) {
        char chck = (char)((node>>(40+8*n)) & 0xFF);
        return chck==0 || (in.read() == chck &&
                           in.isEos() == false);
    }

    private static char chck(long node) {
        return (char)((node>>32) & 0xFF);
    }
    
    private static int base(long node) {
        return (int)(node & 0x1FFFFFFF);
    }

    private static boolean isTerminal(long node) {
        return ((node>>31) & 1)==1;
    }

    private static int type(long node) {
        return (int)((node>>29) & 3);
    }

    private static int siblingTotal(long node) {
        switch (type(node)) {
        case 0:
            return (int)((node>>56) & 0xFF);
        case 1:
            return (int)((node>>48) & 0xFFFF);
        default:
            return (int)((node>>40) & 0xFFFFFF);
        }
    }

    private static long getNode(int i) {
        return (((long)(nodes[i*8+0] & 0xff) << 56) | 
                ((long)(nodes[i*8+1] & 0xff) << 48) |
                ((long)(nodes[i*8+2] & 0xff) << 40) | 
                ((long)(nodes[i*8+3] & 0xff) << 32) | 
                ((long)(nodes[i*8+4] & 0xff) << 24) |
                ((long)(nodes[i*8+5] & 0xff) << 16) |
                ((long)(nodes[i*8+6] & 0xff) <<  8) | 
                ((long)(nodes[i*8+7] & 0xff)));
    }
}
