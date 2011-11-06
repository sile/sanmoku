package net.reduls.sanmoku;

import android.app.Activity;
import android.view.View;
import android.widget.TextView;
import android.text.Spannable;
import android.text.Spanned;
import android.text.SpannableString;
import android.text.style.CharacterStyle;
import android.text.style.ClickableSpan;
import android.content.Intent;
import java.util.List;
import java.util.ArrayList;
import android.graphics.Typeface;
import android.text.style.StyleSpan;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.text.style.UnderlineSpan;
import android.graphics.Color;
import android.text.TextPaint;

public class DataFormatter {
    private static class StyleWithRegion {
        public final CharacterStyle style;
        public final int start;
        public final int end;
        public StyleWithRegion(CharacterStyle style, int start, int end) {
            this.style = style;
            this.start = start;
            this.end = end;
        }
    }
    public static SpannableString format(String text) {
        StringBuilder sb = new StringBuilder();
        List<StyleWithRegion> spans = new ArrayList<StyleWithRegion>();

        format_impl(text, sb, spans);
        
        SpannableString spannable = new SpannableString(sb.toString());
        for(StyleWithRegion s : spans) 
            spannable.setSpan(s.style, s.start, s.end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        return spannable;
    }

    private static void format_impl(String text, StringBuilder sb, List<StyleWithRegion> styles) {
        for(int i = 0; i < text.length(); i++) {
            switch(text.charAt(i)) {
            case '`':
                i = format1(text, i+1, sb, styles);
                break;
            case '{':
                i = format2(text, i+1, sb, styles);
                break;
            case '[':
                i = format3(text, i+1, sb, styles);
                break;
            case '<':
                i = format5(text, i+1, sb, styles);                
                break;
            case '-':
                if(text.substring(i+1).startsWith("ex:")) {
                    i = format4(text, i+1, sb ,styles);
                    break;
                }
            default:
                sb.append(text.charAt(i));
            }
        }
    }

    private static int format1(String text, int start, StringBuilder sb, List<StyleWithRegion> styles) {
        int end = text.indexOf('`', start);
        if(end != -1) {
            styles.add(new StyleWithRegion(new StyleSpan(Typeface.BOLD), sb.length(), sb.length()+end-start));
            sb.append(text.substring(start,end));
        } else {
            end = start;
        }
        return end;
    }

    private static int format2(String text, int start, StringBuilder sb, List<StyleWithRegion> styles) {
        int end = text.indexOf('}', start);
        if(end != -1) {
            styles.add(new StyleWithRegion(new StyleSpan(Typeface.BOLD), sb.length(), sb.length()+end-start));
            styles.add(new StyleWithRegion(new ForegroundColorSpan(Color.YELLOW), sb.length(), sb.length()+end-start));
            sb.append(text.substring(start,end));
        } else {
            end = start;
        }
        return end;
    }

    private static int format3(String text, int start, StringBuilder sb, List<StyleWithRegion> styles) {
        int end = text.indexOf(']', start);
        if(end != -1) {
            styles.add(new StyleWithRegion(new StyleSpan(Typeface.BOLD), sb.length(), sb.length()+end-start));
            styles.add(new StyleWithRegion(new BackgroundColorSpan(Color.BLUE), sb.length(), sb.length()+end-start));
            sb.append(text.substring(start,end));
        } else {
            end = start;
        }
        return end;
    }

    private static int format4(String text, int start, StringBuilder sb, List<StyleWithRegion> styles) {
        int end = text.indexOf('\n', start);
        if(end != -1) {
            styles.add(new StyleWithRegion(new UnderlineSpan(), sb.length()+4, sb.length()+end-start));
            styles.add(new StyleWithRegion(new ForegroundColorSpan(Color.CYAN), sb.length()+4, sb.length()+end-start));
            sb.append(text.substring(start,end)+"\n");
        } else {
            end = start;
        }
        return end;
    }

    private static int format5(String text, int start, StringBuilder sb, List<StyleWithRegion> styles) {
        int end = text.indexOf('>', start);
        if(end != -1) {
            styles.add(new StyleWithRegion(new StyleSpan(Typeface.BOLD), sb.length(), sb.length()+end-start));
            styles.add(new StyleWithRegion(new ForegroundColorSpan(Color.GREEN), sb.length(), sb.length()+end-start));
            sb.append(text.substring(start,end));
        } else {
            end = start;
        }
        return end;
    }

    public static void link (Activity a, String text, Spannable span) {
        int beg = nextNonDelimitorPosition(text, 0);
        int end = nextDelimitorPosition(text, beg);
        while(beg < text.length()) {
            span.setSpan(new WordLinkSpan(a, beg, end), beg, end, 
                         Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
            beg = nextNonDelimitorPosition(text, end);
            end = nextDelimitorPosition(text, beg);
        }
    }

    private static int nextDelimitorPosition(String text, int start) {
        for(int i = start; i < text.length(); i++) {
            switch(text.charAt(i)) {
            case ' ': case ',': case ':': case ';': case '(': case ')': case '━':
            case '!': case '?': case '/': case '—':
            case '"': case '.': case 10: case 13: case 9:
                return i;
            }
        }
        return text.length();
    }

    private static int nextNonDelimitorPosition(String text, int start) {
        for(int i = start; i < text.length(); i++) {
            switch(text.charAt(i)) {
            case ' ': case ',': case ':': case ';': case '(': case ')': case '━':
            case '!': case '?': case '/': case '—':
            case '"': case '.': case 10: case 13: case 9:
                continue;
            default:
                return i;
            }
        }
        return text.length();        
    }

    private static class WordLinkSpan extends ClickableSpan {
        private final Activity activity;
        private final int start;
        private final int end;

        public WordLinkSpan(Activity activity, int start, int end) {
            super();
            this.activity = activity;
            this.start = start;
            this.end = end;
        }
        
        public void onClick(View v) {
            /*
            Intent i = new Intent(activity, Dic.class);
            i.putExtra("search.key", ((TextView)v).getText().toString().substring(start,end));
            activity.startActivity(i);            
            */
        }
        
        public void updateDrawState(TextPaint ds) {}
    }    
}