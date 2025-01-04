package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.AreaType;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.Component;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AreaArgumentType implements ArgumentType<AreaType> {

    private static final Collection<String> EXAMPLES = Stream.of(AreaType.values())
            .map(areaType -> areaType.areaType)
            .collect(Collectors.toSet());

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(Component.translatableWithFallback("cli.arg.area.parse.invalid", "Unable to parse provided area value"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> Component.translatableWithFallback("cli.arg.area.invalid", "Invalid area value provided: '%s'", flag)
    );

    private static final Pattern VALID_AREA_NAME_CHAR_PATTERN = Pattern.compile("^[A-Za-z]$");

    public static AreaArgumentType areaType() {
        return new AreaArgumentType();
    }

    public static AreaType getAreaType(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
        String area = context.getArgument(CommandConstants.AREA.toString(), String.class);
        AreaType areaType = AreaType.of(area);
        if (areaType == null) {
            throw ERROR_INVALID_VALUE.create(areaType);
        } else {
            return areaType;
        }
    }

    @Override
    public AreaType parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();

        while (reader.canRead() && String.valueOf(reader.peek()).matches(VALID_AREA_NAME_CHAR_PATTERN.pattern())) {
            reader.skip();
        }
        String s = reader.getString().substring(i, reader.getCursor());
        try {
            if (AreaType.isValidAreaType(s)) {
                return AreaType.of(s);
            } else {
                throw new IllegalArgumentException("Invalid area type supplied");
            }
        } catch (IllegalArgumentException argumentException) {
            reader.setCursor(i);
            Constants.LOGGER.error("Error parsing area type");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSourceStack) {
            return SharedSuggestionProvider.suggest(AreaType.getTypes(), builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }
}
