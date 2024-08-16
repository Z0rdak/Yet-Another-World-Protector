package de.z0rdak.yawp.commands.arguments.region;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;
import static net.minecraft.ChatFormatting.RED;

public class ContainingOwnedRegionArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");
    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(Component.translatableWithFallback("cli.arg.region.parse.invalid", "Unable to parse region name!"));
    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> Component.translatableWithFallback("cli.arg.region.invalid", "Region '%s' does not exist", flag)
    );

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static ContainingOwnedRegionArgumentType owningRegions() {
        return new ContainingOwnedRegionArgumentType();
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        String containingRegionName = context.getArgument(argName, String.class);
        Player player;
        LocalRegions.RegionOverlappingInfo overlapping;
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(context.getSource().getLevel().dimension());
        IMarkableRegion region = dimCache.getRegion(containingRegionName);
        if (region == null) {
            throw ERROR_INVALID_VALUE.create(containingRegionName);
        }
        try {
            player = context.getSource().getPlayerOrException();
            overlapping = LocalRegions.getOverlappingWithPermission(region, player);
        } catch (CommandSyntaxException e) {
            overlapping = LocalRegions.getOverlappingRegions(region);
        }
        if (overlapping.containingRegions.contains(region)) {
            return region;
        } else {
            sendCmdFeedback(context.getSource(), Component.translatableWithFallback("cli.arg.region.owned.invalid", containingRegionName));
            throw ERROR_INVALID_VALUE.create(containingRegionName);
        }
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();
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
            YetAnotherWorldProtector.LOGGER.error("Error parsing region name");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    /**
     * Suggests regions with permission, which are also fully containing the area provided by the marked blocks of the region marker
     */
    public <S> CompletableFuture<Suggestions> listSuggestionsWithMarker(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSourceStack src) {
            try {
                Player player = src.getPlayerOrException();
                ItemStack maybeStick = player.getMainHandItem();
                if (StickUtil.isVanillaStick(maybeStick) && StickUtil.isMarker(maybeStick)) {
                    CompoundTag stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        MarkerStick marker = new MarkerStick(stickNBT);
                        if (!marker.isValidArea()) {
                            return Suggestions.empty();
                        }
                        IMarkableArea markedArea = StickUtil.getMarkedArea(player.getMainHandItem());
                        LocalRegions.RegionOverlappingInfo overlapping = LocalRegions.getOverlappingWithPermission(markedArea, player);
                        if (!overlapping.hasContaining()) {
                            sendCmdFeedback(src, Component.translatableWithFallback("cli.arg.area.owned.no-containment", "No suitable Local Region as parent for the marked area. Attempting to set Dimensional Region as parent."));
                            return Suggestions.empty();
                        }
                        Set<String> containingRegionName = overlapping.containingRegions.stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
                        return SharedSuggestionProvider.suggest(containingRegionName, builder);
                    }
                }
                return Suggestions.empty();
            } catch (CommandSyntaxException e) {
                YetAnotherWorldProtector.LOGGER.error(e);
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    /**
     * Suggests regions with permission, which are also fully containing the area provided by the create local command
     */
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSourceStack src) {
            CommandContext<CommandSourceStack> ctx = (CommandContext<CommandSourceStack>) context;
            try {
                IMarkableArea markedArea = null;
                AreaType areaType = null;
                if (ctx.getInput().contains(AreaType.CUBOID.areaType)) {
                    areaType = AreaType.CUBOID;
                }
                if (ctx.getInput().contains(AreaType.SPHERE.areaType)) {
                    areaType = AreaType.SPHERE;
                }
                switch (areaType) {
                    case CUBOID:
                        BlockPos p1 = BlockPosArgument.getSpawnablePos(ctx, POS1.toString());
                        BlockPos p2 = BlockPosArgument.getSpawnablePos(ctx, POS2.toString());
                        markedArea = new CuboidArea(p1, p2);
                        break;
                    case SPHERE:
                        try {
                            BlockPos centerPos = BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString());
                            BlockPos radiusPos = BlockPosArgument.getSpawnablePos(ctx, RADIUS_POS.toString());
                            markedArea = new SphereArea(centerPos, radiusPos);
                        } catch (CommandSyntaxException cse) {
                            BlockPos centerPos = BlockPosArgument.getSpawnablePos(ctx, CENTER_POS.toString());
                            int radius = IntegerArgumentType.getInteger(context, RADIUS.toString());
                            markedArea = new SphereArea(centerPos, radius);
                        }
                        break;
                    default:
                        throw new IllegalStateException("Unexpected value: " + areaType);
                }
                if (markedArea == null) {
                    throw new IllegalArgumentException("Could not get marked blocks from command");
                }
                Player player;
                LocalRegions.RegionOverlappingInfo overlapping;
                try {
                    player = src.getPlayerOrException();
                    overlapping = LocalRegions.getOverlappingWithPermission(markedArea, player);
                } catch (CommandSyntaxException e) {
                    overlapping = LocalRegions.getOverlappingRegions(markedArea, src.getLevel().dimension());
                }
                if (!overlapping.hasContaining()) {
                    sendCmdFeedback(src, Component.translatableWithFallback("cli.arg.area.owned.no-containment", "No suitable Local Region as parent for the marked area. Attempting to set Dimensional Region as parent."));
                    return Suggestions.empty();
                }
                Set<String> containingRegionName = overlapping.containingRegions.stream().map(IProtectedRegion::getName).collect(Collectors.toSet());
                return SharedSuggestionProvider.suggest(containingRegionName, builder);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }
}
