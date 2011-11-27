package net.reduls.sanmoku.dic;

import java.util.Iterator;
import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Morpheme {
    private static final byte[] morps;
    private static final byte[] morpMap;
    private static final byte[] leafs;
    private static final byte[] leafAccCounts;
    private static final int nextBase;
    
    static {
        morps = Misc.readBytesFromFile("morp.info.bin", 2);
        morpMap = Misc.readBytesFromFile("morp.info.map", 4);
        leafs = Misc.readBytesFromFile("morp.leaf.bin", 8);
        leafAccCounts = Misc.readBytesFromFile("morp.leaf.cnt.bin", 2);
        nextBase = Misc.readIntFromFile("morp.base.bin");
    }

    public static Iterable<Entry> getMorphemes(final int surfaceId) {
        return new Iterable<Entry> () {
            public Iterator<Entry> iterator() {
                return new MorphemeIterator(surfaceId);
            }
        };
    }

    private static final int nextNode(int index) {
        final long leaf = getLeaf(index);
        if(hasNext(leaf, index)==false)
            return -1;

        return nextNode(leaf, index);
    }

    private static final boolean hasNext(long leaf, int i) {
        final long mask = ((long)1 << (i%64));
        return (leaf & mask)!=0;
    }
    
    private static final int nextNode(long leaf, int i) {
        final int i2 = i/64;
        final int offset = ((int)((leafAccCounts[i2*2+0]&0xFF)<<8) | 
                            (int)((leafAccCounts[i2*2+1]&0xFF)));
        final long mask = ((long)1 << (i%64))-1;
        return nextBase + offset + Long.bitCount(leaf&mask);
    }
    
    private static final long getLeaf(int i2) { 
        final int i = i2/64;
        return (((long)(leafs[i*8+0]) << 56) | 
                ((long)(leafs[i*8+1] & 0xff) << 48) | 
                ((long)(leafs[i*8+2] & 0xff) << 40) | 
                ((long)(leafs[i*8+3] & 0xff) << 32) | 
                ((long)(leafs[i*8+4] & 0xff) << 24) |
                ((long)(leafs[i*8+5] & 0xff) << 16) |
                ((long)(leafs[i*8+6] & 0xff) <<  8) | 
                ((long)(leafs[i*8+7] & 0xff)));
    }

    static class MorphemeIterator implements Iterator<Entry> {
        private int node;

        public MorphemeIterator(int node) {
            this.node = node;
        }
        
        public boolean hasNext() {
            return node != -1;
        }
        
        public Entry next() {
            final Entry e = new Entry(node);
            node = nextNode(node);
            return e;
        }
        
        public void remove() {
            throw new UnsupportedOperationException();  
        }
    }

    static class Entry {
        public final short posId;
        public final short cost;
        public final int morphemeId;
        
        private Entry(int i) {
            final int m = (int)((morps[i*2+0]&0xFF)<<8) | (int)(morps[i*2+1]&0xFF);
            posId = (short)((short)(morpMap[m*4+0]<<8) | (short)(morpMap[m*4+1]&0xFF));
            cost = (short)((short)(morpMap[m*4+2]<<8) | (short)(morpMap[m*4+3]&0xFF));

            morphemeId = i;
        }
    }
}
