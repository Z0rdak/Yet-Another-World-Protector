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
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.REMOVE;
import static de.z0rdak.yawp.util.CommandUtil.getRegionArgument;

public class IFlagArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = RegionFlag.getFlagNames();

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslationTextComponent("cli.arg.flag.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslationTextComponent("cli.arg.flag.invalid", flag)
    );

    public static final Pattern VALID_FLAG_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z\\-][A-Za-z]$");

    private IFlagArgumentType(){}

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
    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            try {
                List<String> flagToSuggest;
                IMarkableRegion region = getRegionArgument((CommandContext<CommandSource>) context);
                boolean isRemoveCmd = context.getNodes()
                        .stream()
                        .map(node -> node.getNode().getName())
                        .collect(Collectors.toSet())
                        .contains(REMOVE.toString());
                List<String> flagsInRegion = region.getFlags()
                        .stream()
                        .map(IFlag::getName)
                        .distinct()
                        .collect(Collectors.toList());
                if (isRemoveCmd) {
                    // Only show existing flags
                    flagToSuggest = flagsInRegion;
                } else {
                    // show flags not in region
                    List<String> allFlags = RegionFlag.getFlagNames();
                    allFlags.removeAll(flagsInRegion);
                    flagToSuggest = allFlags;
                }
                if (isRemoveCmd && flagToSuggest.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, new StringTextComponent("No flags defined in region '" + region.getName() + "'!"));
                    return Suggestions.empty();
                }
                if (!isRemoveCmd && flagToSuggest.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, new StringTextComponent("Region '" + region.getName() + "' already contains all flags!"));
                    return Suggestions.empty();
                }
                return ISuggestionProvider.suggest(flagToSuggest, builder);
            } catch (CommandSyntaxException e) {
                throw new RuntimeException(e);
            }

        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static IFlagArgumentType flag() {
        return new IFlagArgumentType();
    }

    public static IFlag getFlag(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String flagIdentifier = context.getArgument(argName, String.class);
        if (RegionFlag.contains(flagIdentifier)) {
            IMarkableRegion region = getRegionArgument(context);
            if (region.containsFlag(flagIdentifier)) {
                return region.getFlag(flagIdentifier);
            } else {
                MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("Region '" + region.getName() + "' does not contain flag '" + flagIdentifier + "'!"));
                // Should not happen!
                throw new IllegalArgumentException("Region '" + region.getName() + "' does not contain flag '" + flagIdentifier + "'!");
            }
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("Invalid flag identifier: '" + flagIdentifier + "'!"));
            throw ERROR_INVALID_VALUE.create(flagIdentifier);
        }
    }
}
