package de.z0rdak.yawp.commands.arguments.flag;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

// TODO: When flag types are implemented there must be different ArgumentTypes for each flag type
public class FlagArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = RegionFlag.getFlagNames();

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslatableComponent("cli.arg.flag.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslatableComponent("cli.arg.flag.invalid", flag)
    );

    public static final Pattern VALID_FLAG_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z\\-][A-Za-z]$");

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();

        while (reader.canRead() && String.valueOf(reader.peek()).matches(Pattern.compile("^[A-Za-z\\d\\-]$").pattern())) {
            reader.skip();
        }
        String s = reader.getString().substring(i, reader.getCursor());

        try {
            boolean isValidName = s.matches(VALID_FLAG_PATTERN.pattern());
            if (isValidName) {
                return s;
            } else {
                throw new IllegalArgumentException("Invalid flag identifier supplied");
            }
        } catch (IllegalArgumentException argumentException) {
            reader.setCursor(i);
            YetAnotherWorldProtector.LOGGER.error("Error parsing flag identifier");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof SharedSuggestionProvider) {
            return SharedSuggestionProvider.suggest(RegionFlag.getFlagNames(), builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    public static FlagArgumentType flag() {
        return new FlagArgumentType();
    }

    public static IFlag getFlag(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        String flagIdentifier = context.getArgument(argName, String.class);
        if (RegionFlag.contains(flagIdentifier)) {
            return Arrays.stream(RegionFlag.values())
                    .filter(flag -> flag.flag.getFlagIdentifier().equals(flagIdentifier))
                    .map(flag -> flag.flag)
                    .collect(Collectors.toList()).get(0);
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new TextComponent("Invalid flag identifier: '" + flagIdentifier + "'!"));
            throw ERROR_INVALID_VALUE.create(flagIdentifier);
        }
    }
}
