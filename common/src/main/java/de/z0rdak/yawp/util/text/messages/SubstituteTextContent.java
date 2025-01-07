package de.z0rdak.yawp.util.text.messages;

import com.google.common.collect.ImmutableList;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.datafixers.util.Either;
import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.*;
import net.minecraft.util.ExtraCodecs;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.mojang.serialization.Codec.STRING;

@Deprecated(since = "Not used until YAWP is required on client-side", forRemoval = false)
public class SubstituteTextContent implements ComponentContents {
    private static final FormattedText TEXT_PERCENT ;
    private static final FormattedText TEXT_NULL;
    private static final Pattern SUBSTITUTE_PATTERN;
    private static final Codec<Object> PRIMITIVE_ARG_CODEC;
    private static final Codec<Object> ARG_CODEC;
    public static final SubstituteTextContent.Type<SubstituteTextContent> TYPE;
    public static final MapCodec<SubstituteTextContent> CODEC;
    
    private final String pattern;
    @Nullable
    private final Object[] args;
    @Nullable
    private List<FormattedText> substitutes = ImmutableList.of();

    private static Optional<List<Object>> adjustArgs(Object[] args) {
        return args.length == 0 ? Optional.empty() : Optional.of(Arrays.asList(args));
    }   
    
    public static final Object[] NO_ARGS = new Object[0];

    private static Object[] adjustArgs(Optional<List<Object>> args) {
        return args.map((arg) -> arg.isEmpty() ? NO_ARGS : arg.toArray()).orElse(NO_ARGS);
    }
    
    private static SubstituteTextContent create(String pattern, Optional<List<Object>> args) {
        return new SubstituteTextContent(pattern, adjustArgs(args));
    }


    private static DataResult<Object> filterAllowedArguments(@Nullable Object input) {
        return !isAllowedPrimitiveArgument(input) 
                ? DataResult.error(() -> "This value needs to be parsed as component") 
                : DataResult.success(input);
    }

    public static boolean isAllowedPrimitiveArgument(@Nullable Object input) {
        return input instanceof Number || input instanceof Boolean || input instanceof String;
    }
    
    static {
        PRIMITIVE_ARG_CODEC = ExtraCodecs.JAVA.validate(SubstituteTextContent::filterAllowedArguments);
        ARG_CODEC = Codec
                .either(PRIMITIVE_ARG_CODEC, ComponentSerialization.CODEC)
                .xmap(objectOrComponent -> objectOrComponent.map(
                        object -> object, 
                    component -> Objects.requireNonNullElse(component.tryCollapseToString(), component)),
                        SubstituteTextContent::getEither);
        CODEC = RecordCodecBuilder
                .mapCodec((stcInstance) -> stcInstance.group(
                        STRING.fieldOf("pattern")
                                .forGetter(stc -> stc.pattern), 
                        ARG_CODEC.listOf()
                                .optionalFieldOf("args")
                                .forGetter(stc -> adjustArgs(stc.args)))
                .apply(stcInstance, SubstituteTextContent::create));
        TYPE = new SubstituteTextContent.Type<>(CODEC, "substitutable");
        TEXT_PERCENT = FormattedText.of("%");
        TEXT_NULL = FormattedText.of("null");
        SUBSTITUTE_PATTERN = Pattern.compile("%(?:(\\d+)\\$)?([A-Za-z%]|$)");
    }

    private static @NotNull Either<Object, Component> getEither(Object either) {
        Either<Object, Component> result;
        if (either instanceof Component component) {
            result = Either.right(component);
        } else {
            result = Either.left(either);
        }
        return result;
    }

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
        Matcher matcher = SUBSTITUTE_PATTERN.matcher(substitute);
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
                    partsConsumer.accept(TEXT_PERCENT);
                } else if ("s".equals(string)) {
                    String string3 = matcher.group(1);
                    int m = string3 != null ? Integer.parseInt(string3) - 1 : i++;
                    partsConsumer.accept(this.getArg(m));
                } else {
                    throw new SubstituteException(this, "Unsupported format: '" + string2 + "'");
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
        return object == null ? TEXT_NULL : FormattedText.of(object.toString());
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

    @Override
    public SubstituteTextContent.Type<SubstituteTextContent> type() {
        return TYPE;
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


