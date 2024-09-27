package de.z0rdak.yawp.util.text.messages;

import com.google.common.collect.ImmutableList;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.*;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SubstituteTextContent implements ComponentContents {
    private static final FormattedText LITERAL_PERCENT_SIGN = FormattedText.of("%");
    private static final FormattedText NULL_ARGUMENT = FormattedText.of("null");
    private static final Pattern ARG_FORMAT = Pattern.compile("%(?:(\\d+)\\$)?([A-Za-z%]|$)");
    private final String pattern;
    @Nullable
    private final Object[] args;
    @Nullable
    private List<FormattedText> substitutes = ImmutableList.of();

    public SubstituteTextContent(String pattern, Object[] args) {
        this.pattern = pattern;
        this.args = args;
    }

    private void updateSubstitutes() {
        try {
            ImmutableList.Builder<FormattedText> builder = ImmutableList.builder();
            this.forEachPart(pattern, builder::add);
            this.substitutes = builder.build();
        } catch (SubstituteException substituteException) {
            this.substitutes = ImmutableList.of(FormattedText.of(pattern));
        }
    }

    private void forEachPart(String substitute, Consumer<FormattedText> partsConsumer) {
        Matcher matcher = ARG_FORMAT.matcher(substitute);
        try {
            int i = 0;
            int j = 0;
            while (matcher.find(j)) {
                String string;
                int k = matcher.start();
                int l = matcher.end();
                if (k > j) {
                    string = substitute.substring(j, k);
                    if (string.indexOf(37) != -1) {
                        throw new IllegalArgumentException();
                    }
                    partsConsumer.accept(FormattedText.of(string));
                }
                string = matcher.group(2);
                String string2 = substitute.substring(k, l);
                if ("%".equals(string) && "%%".equals(string2)) {
                    partsConsumer.accept(LITERAL_PERCENT_SIGN);
                } else if ("s".equals(string)) {
                    String string3 = matcher.group(1);
                    int m = string3 != null ? Integer.parseInt(string3) - 1 : i++;
                    partsConsumer.accept(this.getArg(m));
                } else {
                    throw new de.z0rdak.yawp.util.text.messages.SubstituteException(this, "Unsupported format: '" + string2 + "'");
                }
                j = l;
            }
            if (j < substitute.length()) {
                String string4 = substitute.substring(j);
                if (string4.indexOf(37) != -1) {
                    throw new IllegalArgumentException();
                }
                partsConsumer.accept(FormattedText.of(string4));
            }
        } catch (IllegalArgumentException illegalArgumentException) {
            throw new SubstituteException(this, illegalArgumentException);
        }
    }

    public final FormattedText getArg(int index) {
        if (index < 0 || index >= this.args.length) {
            throw new SubstituteException(this, index);
        }
        Object object = this.args[index];
        if (object instanceof Component) {
            return (Component) object;
        }
        return object == null ? NULL_ARGUMENT : FormattedText.of(object.toString());
    }

    @Override
    public <T> Optional<T> visit(FormattedText.StyledContentConsumer<T> visitor, Style style) {
        this.updateSubstitutes();
        if (this.substitutes == null)
            return Optional.empty();
        for (FormattedText stringVisitable : this.substitutes) {
            Optional<T> optional = stringVisitable.visit(visitor, style);
            if (optional.isEmpty()) continue;
            return optional;
        }
        return Optional.empty();
    }

    @Override
    public <T> Optional<T> visit(FormattedText.ContentConsumer<T> visitor) {
        this.updateSubstitutes();
        if (this.substitutes == null)
            return Optional.empty();
        for (FormattedText stringVisitable : this.substitutes) {
            Optional<T> optional = stringVisitable.visit(visitor);
            if (optional.isEmpty()) continue;
            return optional;
        }
        return Optional.empty();
    }

    @Override
    public MutableComponent resolve(@Nullable CommandSourceStack source, @Nullable Entity sender, int depth) throws CommandSyntaxException {
        Object[] objects = new Object[this.args.length];
        for (int i = 0; i < objects.length; ++i) {
            Object object = this.args[i];
            objects[i] = object instanceof Component ? ComponentUtils.updateForEntity(source, (Component) object, sender, depth) : object;
        }
        return MutableComponent.create(new SubstituteTextContent(this.pattern, objects));
    }

    /*
     * Enabled force condition propagation
     * Lifted jumps to return sites
     */
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SubstituteTextContent substituteTextContent)) return false;
        if (!Objects.equals(this.pattern, substituteTextContent.pattern)) return false;
        return Arrays.equals(this.args, substituteTextContent.args);
    }

    public int hashCode() {
        int i = Objects.hashCode(this.pattern);
        i = 31 * i + Arrays.hashCode(this.args);
        return i;
    }

    public String toString() {
        return "substitute{key='" + this.pattern + "', args=" + Arrays.toString(this.args) + "}";
    }

    public String getPattern() {
        return this.pattern;
    }


    public Object[] getArgs() {
        return this.args;
    }
}


