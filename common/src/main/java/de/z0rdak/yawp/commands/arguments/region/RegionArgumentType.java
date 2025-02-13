package de.z0rdak.yawp.commands.arguments.region;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.context.ParsedCommandNode;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.commands.CommandConstants;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.DimensionRegionCache;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.api.commands.CommandConstants.FLAG;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;

public class RegionArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");
    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(Component.translatableWithFallback("cli.arg.region.parse.invalid", "Unable to parse region name!"));
    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> Component.translatableWithFallback("cli.arg.region.invalid", "Region '%s' does not exist", flag)
    );

    public static <S> RegionType getRegionType(CommandContext<S> context) {
        List<ParsedCommandNode<S>> nodes = context.getNodes();
        if (nodes.size() >= 2) {
            String baseCmd = nodes.get(0).getNode().getName();
            if (baseCmd.equals(Constants.MOD_ID)) {
                String regionTypeLiteral = nodes.get(1).getNode().getName();
                RegionType regionType = RegionType.of(regionTypeLiteral);
                boolean isFlagSubCmd = regionTypeLiteral.equals(FLAG.toString()) && nodes.size() >= 3;

                if (isFlagSubCmd && regionType == null) {
                    String flagRegionTypeLiteral = nodes.get(2).getNode().getName();
                    return RegionType.of(flagRegionTypeLiteral);
                }
                if (!isFlagSubCmd && regionType != null) {
                    return regionType;
                }
            }
        }
        return null;
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(context);
        if (!dimCache.contains(regionName)) {
            sendCmdFeedback(context.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            sendCmdFeedback(context.getSource(), Component.literal("No regions defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    public static IProtectedRegion getRegion(CommandContext<CommandSourceStack> ctx, RegionType regionType) throws CommandSyntaxException {
        switch (regionType) {
            case GLOBAL:
                return RegionDataManager.get().getGlobalRegion();
            case DIMENSION: {
                DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(ctx);
                return dimCache.getDimensionalRegion();
            }
            case LOCAL: {
                DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(ctx);
                String regionName = ctx.getArgument(CommandConstants.LOCAL.toString(), String.class);
                if (!dimCache.contains(regionName)) {
                    sendCmdFeedback(ctx.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
                IMarkableRegion region = dimCache.getRegion(regionName);
                if (region != null) {
                    return region;
                } else {
                    sendCmdFeedback(ctx.getSource(), Component.literal("No regions defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
                    throw ERROR_INVALID_VALUE.create(regionName);
                }
            }
            default:
                throw ERROR_INVALID_VALUE.create("");
        }
    }

    public static IProtectedRegion getTargetRegion(CommandContext<CommandSourceStack> ctx, String argName) throws CommandSyntaxException {
        String regionName = ctx.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getTargetDimRegionArgument(ctx);
        if (!dimCache.contains(regionName)) {
            sendCmdFeedback(ctx.getSource(), Component.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            sendCmdFeedback(ctx.getSource(), Component.literal("No regions defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static RegionArgumentType region() {
        return new RegionArgumentType();
    }

    public static IMarkableRegion getRegionInPlayerDim(CommandContext<CommandSourceStack> ctx, String argName) throws CommandSyntaxException {
        String regionName = ctx.getArgument(argName, String.class);
        ServerPlayer player = ctx.getSource().getPlayerOrException();
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.level().dimension());
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            sendCmdFeedback(ctx.getSource(), Component.literal("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();
        // Pattern only matches chars, not the valid name
        while (reader.canRead() && String.valueOf(reader.peek()).matches(Pattern.compile("^[A-Za-z\\d\\-]$").pattern())) {
            reader.skip();
        }
        String s = reader.getString().substring(i, reader.getCursor());

        try {
            boolean isValidName = s.matches(VALID_NAME_PATTERN.pattern());
            if (isValidName) {
                return s;
            } else {
                throw new IllegalArgumentException("Invalid region name supplied");
            }
        } catch (IllegalArgumentException argumentException) {
            reader.setCursor(i);
            Constants.LOGGER.error("Error parsing region name");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> ctx, SuggestionsBuilder builder) {
        if (ctx.getSource() instanceof CommandSourceStack src) {
            DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument((CommandContext<CommandSourceStack>) ctx);
            return suggestRegionsForOwner(builder, src, dimCache);
        } else {
            return Suggestions.empty();
        }
    }

    private CompletableFuture<Suggestions> suggestRegionsForOwner(SuggestionsBuilder builder, CommandSourceStack src, DimensionRegionCache dimCache) {
        Collection<IMarkableRegion> regions = dimCache.getAllLocal();
        boolean hasPermission = Permissions.get().hasCmdPermission(src);
        if (hasPermission) {
            Collection<String> regionNames = dimCache.getRegionNames();
            if (regionNames.isEmpty()) {
                sendCmdFeedback(src, Component.literal("No regions defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
                return Suggestions.empty();
            } else {
                return SharedSuggestionProvider.suggest(regionNames, builder);
            }
        } else {
            if (src.getEntity() instanceof Player player) {
                regions = regions.stream()
                        .filter(region -> Permissions.get().hasOwnerPermission(region, player))
                        .collect(Collectors.toList());
                Collection<String> regionNames = regions.stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
                if (regionNames.isEmpty()) {
                    sendCmdFeedback(src, Component.literal("No regions defined in dim '" + dimCache.getDimensionalRegion().getName() + "'"));
                    return Suggestions.empty();
                }
                return SharedSuggestionProvider.suggest(regionNames, builder);
            }
        }
        return Suggestions.empty();
    }

    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listRegionsInTargetDim(CommandContext<S> ctx, SuggestionsBuilder builder) {
        if (ctx.getSource() instanceof CommandSourceStack src) {
            try {
                DimensionRegionCache dimCache = ArgumentUtil.getTargetDimRegionArgument((CommandContext<CommandSourceStack>) ctx);
                return suggestRegionsForOwner(builder, src, dimCache);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }
}
