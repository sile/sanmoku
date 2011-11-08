package net.reduls.sanmoku.dic;

import java.util.Iterator;
import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Morpheme {
    private static final byte[] posidMap;
    private static final byte[] costMap;
    private static final byte[] morps;
    private static final byte[] morpExts;

    static {
        posidMap = Misc.readBytesFromFile("morp.posid.map", 2);
        costMap = Misc.readBytesFromFile("morp.cost.map", 2);
        morps = Misc.readBytesFromFile("morp.root.bin", 3);
        morpExts = Misc.readBytesFromFile("morp.ext.bin", 4);
    }
    
    public static int morphemeNode(int surfaceId) {
        final int i = surfaceId;
        return 
            (int)((morps[i*3+0]&0xFF)<<16) |
            (int)((morps[i*3+1]&0xFF)<<8) |
            (int)((morps[i*3+2]&0xFF)<<0);
    }

    public static int morphemeCount(int node) {
        if((node >> 23)==1)
            return 1;
        else
            return (node >> 18);
    }

    public static short rootPosId(int node) {
        final int i = (node>>14) & 0x1FF;
        return (short)((posidMap[i*2]<<8) | (posidMap[i*2+1]&0xFF));
    }

    public static short rootCost(int node) {
        final int i = node & 0x3FFF;
        return (short)((costMap[i*2]<<8) | (costMap[i*2+1]&0xFF));
    }

    public static Iterable<Entry> getMorphemes(int surfaceId) {
        final int node = morphemeNode(surfaceId);
        final int count = morphemeCount(node);

        return new Iterable<Entry> () {
            public Iterator<Entry> iterator() {
                return count==1 ? new SingleIterator(node) : new MultiIterator(node,count);
            }
        };
    }

    static class SingleIterator implements Iterator<Entry> {
        private final int node;
        private int count = 1;

        public SingleIterator(int node) {
            this.node = node;
        }
        
        public boolean hasNext() {
            return count > 0;
        }
        
        public Entry next() {
            count--;
            return new Entry(rootPosId(node), rootCost(node));
        }
        
        public void remove() {
            throw new UnsupportedOperationException();  
        }
    }

    static class MultiIterator implements Iterator<Entry> {
        private int position;
        private int count;

        public MultiIterator(int node, int count) {
            this.position = node & 0x3FFFF;
            this.count = count;
        }
        
        public boolean hasNext() {
            return count > 0;
        }
        
        public Entry next() {
            final int i = position*4;
            final short posId = (short)((morpExts[i+0]<<8) | (morpExts[i+1]&0xFF));
            final short cost = (short)((morpExts[i+2]<<8) | (morpExts[i+3]&0xFF));
            count--;
            position++;
            return new Entry(posId, cost);
        }
        
        public void remove() {
            throw new UnsupportedOperationException();  
        }
    }

    static class Entry {
        public final short posId;
        public final short cost;
        
        private Entry(short posId, short cost) {
            this.posId = posId;
            this.cost = cost;
        }
    }
}
