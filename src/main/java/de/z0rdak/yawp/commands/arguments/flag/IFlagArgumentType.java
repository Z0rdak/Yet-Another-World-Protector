package de.z0rdak.yawp.commands.arguments.flag;

import com.mojang.brigadier.LiteralMessage;
import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandConstants;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.ADD;
import static de.z0rdak.yawp.commands.CommandConstants.REMOVE;

public class IFlagArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = RegionFlag.getFlagNames();

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslatableComponent("cli.arg.flag.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslatableComponent("cli.arg.flag.invalid", flag)
    );

    public static final Pattern VALID_FLAG_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z\\-][A-Za-z]$");

    private IFlagArgumentType() {
    }

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

    public static IFlag getFlag(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        RegionType regionType = RegionArgumentType.getRegionType(context);
        String flagIdentifier = context.getArgument(argName, String.class);
        if (RegionFlag.contains(flagIdentifier) && regionType != null) {
            IProtectedRegion region = RegionArgumentType.getRegion(context, regionType);
            if (region.containsFlag(flagIdentifier)) {
                return region.getFlag(flagIdentifier);
            } else {
                MessageUtil.sendCmdFeedback(context.getSource(), new TextComponent("Region '" + region.getName() + "' does not contain flag '" + flagIdentifier + "'!"));
                // Should not happen!
                throw new IllegalArgumentException("Region '" + region.getName() + "' does not contain flag '" + flagIdentifier + "'!");
            }
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new TextComponent("Invalid flag identifier: '" + flagIdentifier + "'!"));
            throw ERROR_INVALID_VALUE.create(flagIdentifier);
        }
    }

    private <S> FlagEditType getEditType(CommandContext<S> context) {
        List<String> nodes = context.getNodes()
                .stream()
                .map(node -> node.getNode().getName())
                .collect(Collectors.toList());
        if (nodes.get(1).equals(CommandConstants.FLAG.toString())) {
            return FlagEditType.INFO;
        }
        if (nodes.contains(ADD.toString())) {
            return FlagEditType.ADD;
        }
        if (nodes.contains(REMOVE.toString())) {
            return FlagEditType.REMOVE;
        }
        return FlagEditType.UNSET;
    }

    private <S> List<String> getSuggestionFlags(FlagEditType flagEditType, IProtectedRegion region) throws CommandSyntaxException {
        List<String> flagsInRegion = region.getFlags()
                .stream()
                .map(IFlag::getName)
                .distinct()
                .collect(Collectors.toList());
        switch (flagEditType) {
            case ADD: // show flags not in region
                List<String> flags = RegionFlag.getFlagNames();
                flags.removeAll(flagsInRegion);
                return flags;
            case REMOVE: // Only show existing flags
            case INFO:
                return flagsInRegion;
            case UNSET:
            default:
                return new ArrayList<>();
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        RegionType regionType = RegionArgumentType.getRegionType(context);
        boolean isCommandSource = context.getSource() instanceof CommandSourceStack;
        if (regionType == null) {
            if (isCommandSource) {
                MessageUtil.sendCmdFeedback((CommandSourceStack) context.getSource(), new TextComponent("Invalid region type supplied"));
            }
            return Suggestions.empty();
        }
        if (isCommandSource) {
            CommandSourceStack src = (CommandSourceStack) context.getSource();
            try {
                IProtectedRegion region = RegionArgumentType.getRegion((CommandContext<CommandSourceStack>) context, regionType);
                FlagEditType flagEditType = getEditType(context);
                List<String> flagToSuggest = getSuggestionFlags(flagEditType, region);
                if ((flagEditType == FlagEditType.REMOVE || flagEditType == FlagEditType.INFO) && flagToSuggest.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, new TextComponent("No flags defined in region '" + region.getName() + "'!"));
                    return Suggestions.empty();
                }
                if (flagEditType == FlagEditType.ADD && flagToSuggest.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, new TextComponent("Region '" + region.getName() + "' already contains all flags!"));
                    return Suggestions.empty();
                }
                return SharedSuggestionProvider.suggest(flagToSuggest, builder);
            } catch (CommandSyntaxException e) {
                throw new RuntimeException(e);
            }

        } else {
            return Suggestions.empty();
        }
    }
}
