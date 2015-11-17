package im.actor.core.api;
/*
 *  Generated by the Actor API Scheme generator.  DO NOT EDIT!
 */

import im.actor.runtime.bser.*;
import im.actor.runtime.collections.*;
import static im.actor.runtime.bser.Utils.*;
import im.actor.core.network.parser.*;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.NotNull;
import com.google.j2objc.annotations.ObjectiveCName;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

public class ApiTextModernAttach extends BserObject {

    private String title;
    private String titleUrl;
    private ApiImageLocation titleIcon;
    private String text;
    private ApiParagraphStyle style;
    private List<ApiTextModernField> fields;

    public ApiTextModernAttach(@Nullable String title, @Nullable String titleUrl, @Nullable ApiImageLocation titleIcon, @Nullable String text, @Nullable ApiParagraphStyle style, @NotNull List<ApiTextModernField> fields) {
        this.title = title;
        this.titleUrl = titleUrl;
        this.titleIcon = titleIcon;
        this.text = text;
        this.style = style;
        this.fields = fields;
    }

    public ApiTextModernAttach() {

    }

    @Nullable
    public String getTitle() {
        return this.title;
    }

    @Nullable
    public String getTitleUrl() {
        return this.titleUrl;
    }

    @Nullable
    public ApiImageLocation getTitleIcon() {
        return this.titleIcon;
    }

    @Nullable
    public String getText() {
        return this.text;
    }

    @Nullable
    public ApiParagraphStyle getStyle() {
        return this.style;
    }

    @NotNull
    public List<ApiTextModernField> getFields() {
        return this.fields;
    }

    @Override
    public void parse(BserValues values) throws IOException {
        this.title = values.optString(1);
        this.titleUrl = values.optString(2);
        this.titleIcon = values.optObj(3, new ApiImageLocation());
        this.text = values.optString(4);
        this.style = values.optObj(5, new ApiParagraphStyle());
        List<ApiTextModernField> _fields = new ArrayList<ApiTextModernField>();
        for (int i = 0; i < values.getRepeatedCount(6); i ++) {
            _fields.add(new ApiTextModernField());
        }
        this.fields = values.getRepeatedObj(6, _fields);
        if (values.hasRemaining()) {
            setUnmappedObjects(values.buildRemaining());
        }
    }

    @Override
    public void serialize(BserWriter writer) throws IOException {
        if (this.title != null) {
            writer.writeString(1, this.title);
        }
        if (this.titleUrl != null) {
            writer.writeString(2, this.titleUrl);
        }
        if (this.titleIcon != null) {
            writer.writeObject(3, this.titleIcon);
        }
        if (this.text != null) {
            writer.writeString(4, this.text);
        }
        if (this.style != null) {
            writer.writeObject(5, this.style);
        }
        writer.writeRepeatedObj(6, this.fields);
        if (this.getUnmappedObjects() != null) {
            SparseArray<Object> unmapped = this.getUnmappedObjects();
            for (int i = 0; i < unmapped.size(); i++) {
                int key = unmapped.keyAt(i);
                writer.writeUnmapped(key, unmapped.get(key));
            }
        }
    }

    @Override
    public String toString() {
        String res = "struct TextModernAttach{";
        res += "title=" + this.title;
        res += ", titleUrl=" + this.titleUrl;
        res += ", titleIcon=" + this.titleIcon;
        res += ", text=" + this.text;
        res += ", style=" + this.style;
        res += "}";
        return res;
    }

}