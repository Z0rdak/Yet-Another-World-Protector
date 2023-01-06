package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;

public class DimensionCacheArgumentType implements ArgumentType<DimensionRegionCache> {

    private static final Collection<String> EXAMPLES = RegionDataManager.get().getDimensionList();

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            dim -> new TranslationTextComponent("cli.arg.dim.invalid", dim)
    );

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     *
     * @return
     */
    public static DimensionCacheArgumentType dimRegion() {
        return new DimensionCacheArgumentType();
    }

    private static DimensionRegionCache getDimensionalRegionCache(ResourceLocation resourcelocation) {
        RegistryKey<World> registrykey = RegistryKey.create(Registry.DIMENSION_REGISTRY, resourcelocation);
        if (RegionDataManager.get().containsCacheFor(registrykey)) {
            return RegionDataManager.get().cacheFor(registrykey);
        } else {
            return RegionDataManager.get().newCacheFor(registrykey);
        }
    }

    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof ISuggestionProvider) {
            return ISuggestionProvider.suggest(RegionDataManager.get().getDimensionList(), builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    public static DimensionRegionCache getDimRegion(CommandContext<CommandSource> context, String dim) throws CommandSyntaxException {
        ResourceLocation resourcelocation = context.getArgument(dim, ResourceLocation.class);
        boolean isValidDimResourceLocation = context.getSource().levels().stream().map(RegistryKey::location).anyMatch(loc -> loc.equals(resourcelocation));
        if (isValidDimResourceLocation) {
            DimensionRegionCache dimCache = getDimensionalRegionCache(resourcelocation);
            if (dimCache == null) {
                throw ERROR_INVALID_VALUE.create(resourcelocation.toString());
            }
            return dimCache;
        } else {
            throw ERROR_INVALID_VALUE.create(resourcelocation.toString());
        }

    }

    @Override
    public DimensionRegionCache parse(StringReader reader) throws CommandSyntaxException {
        ResourceLocation resourcelocation = ResourceLocation.read(reader);
        RegistryKey<World> registrykey = RegistryKey.create(Registry.DIMENSION_REGISTRY, resourcelocation);
        if (RegionDataManager.get().containsCacheFor(registrykey)) {
            return RegionDataManager.get().cacheFor(registrykey);
        }
        return null;
    }
}
