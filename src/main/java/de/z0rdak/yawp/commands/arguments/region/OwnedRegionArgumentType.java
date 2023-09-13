package de.z0rdak.yawp.commands.arguments.region;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.RegionCommands;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.util.MessageUtil;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.StickUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class OwnedRegionArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");
    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslationTextComponent("cli.arg.region.parse.invalid"));
    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslationTextComponent("cli.arg.region.invalid", flag)
    );

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static OwnedRegionArgumentType region() {
        return new OwnedRegionArgumentType();
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument(context);
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("No region with name '" + argName + "' defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();

        // FIXME: Pattern only matches chars, not the valid name
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

    @SuppressWarnings("unchecked")
    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            try {
                ServerPlayerEntity player = src.getPlayerOrException();
                ItemStack maybeStick = player.getMainHandItem();
                if (StickUtil.isVanillaStick(maybeStick)) {
                    StickType stickType = StickUtil.getStickType(maybeStick);
                    if (stickType == StickType.MARKER) {
                        CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                        if (stickNBT != null) {
                            MarkerStick marker = new MarkerStick(stickNBT);
                            if (!marker.isValidArea()) {
                                return Suggestions.empty();
                            }
                            CuboidArea markedArea = new CuboidArea(marker.getMarkedBlocks().get(0), marker.getMarkedBlocks().get(1));
                            DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.level.dimension());
                            List<String> ownedRegions = dimCache.getRegions()
                                    .stream()
                                    .filter(r -> r.hasPlayer(player.getUUID(), RegionCommands.OWNER))
                                    .filter(r -> ((CuboidArea) r.getArea()).contains(markedArea))
                                    .map(IMarkableRegion::getName)
                                    .collect(Collectors.toList());
                            if (ownedRegions.isEmpty()) {
                                MessageUtil.sendCmdFeedback(src, new TranslationTextComponent("You don't have owner permissions for any region in this dimension!'"));
                                return Suggestions.empty();
                            }
                            return ISuggestionProvider.suggest(ownedRegions, builder);
                        }
                    }
                }
                return Suggestions.empty();
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
