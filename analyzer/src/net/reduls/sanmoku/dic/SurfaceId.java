package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class SurfaceId {
    private static final int idOffset;
    private static final byte[] nodes;
    private static final byte[] exts;
    private static final byte[] char_to_chck;
    
    static {
        nodes = Misc.readBytesFromFile("surface-id.bin.node", 1);
        exts = Misc.readBytesFromFile("surface-id.bin.ext", 1);
        char_to_chck = Misc.readBytesFromFile("surface-id.bin.char", 0x100, 1);
        idOffset = Misc.readIntFromFile("category.bin");
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
            
            final char arc = read(in);
            final long next = getNode(base(node)+arc);
            if(chck(next) != arc)
                return;
            node = next;
            id += siblingTotal(node);
        }
    }

    private static char read(CodeStream in) {
        return (char)(char_to_chck[in.read()]&0xFF);
    }

    private static boolean checkEncodedChildren(CodeStream in, long node) {
        switch(type(node)) {
        case 0:
            return checkEC(in,node);
        default:
            return true;
        }
    }
    private static boolean checkEC(CodeStream in, long node) {
        char chck = (char)((node>>27) & 0x7F);
        return chck==0 || (read(in) == chck &&
                           in.isEos() == false);
    }

    private static char chck(long node) {
        return (char)((node>>20) & 0x7F);
    }
    
    private static int base(long node) {
        return (int)(node & 0x7FFFF);
    }

    private static boolean isTerminal(long node) {
        return ((node>>19) & 1)==1;
    }

    private static int type(long node) {
        if (((node>>39) & 1)==1) {
            return 2+(int)((node>>38) & 1);
        } else {
            return 0;
        }
    }

    private static int siblingTotal(long node) {
        switch (type(node)) {
        case 0:
            return (int)((node>>34) & 0x1F);
        case 2:
            return (int)((node>>27) & 0x7FF);
        default:
            {
                int i = (int)((node>>27) & 0x7FF);
                return 
                    (int)((exts[i*4+0]&0xFF)<<24) |
                    (int)((exts[i*4+1]&0xFF)<<16) |
                    (int)((exts[i*4+2]&0xFF)<<8) |
                    (int)((exts[i*4+3]&0xFF)<<0);
            }
        }
    }

    private static long getNode(int i) {
        return (((long)(nodes[i*5+0] & 0xff) << 32) | 
                ((long)(nodes[i*5+1] & 0xff) << 24) |
                ((long)(nodes[i*5+2] & 0xff) << 16) |
                ((long)(nodes[i*5+3] & 0xff) <<  8) | 
                ((long)(nodes[i*5+4] & 0xff)));
    }
}
